module Application
    ( getApplicationDev
    , appMain
    , develMain
    , makeFoundation
    , makeLogWare
    -- * for DevelMain
    , getApplicationRepl
    , shutdownApp
    -- * for GHCI
    , handler
    , db
    ) where

#if DEVELOPMENT
import           LoadEnv
#endif

import qualified Migrations
import           Control.Concurrent (forkIO)
import           Control.Monad.Logger (liftLoc, runLoggingT)
import           Data.Time.Clock
import           Database.Persist.Sql
import           Import
import           Database.Persist.Postgresql (createPostgresqlPool, pgConnStr, pgPoolSize)
import           Language.Haskell.TH.Syntax (qLocation)
import           Network.Wai (Middleware)
import           Network.Wai.Middleware.AcceptOverride
import           Network.Wai.Middleware.Autohead
import           Network.Wai.Middleware.MethodOverride
import           Network.Wai.Middleware.Cors
import           Yesod.Core.Types (loggerSet)
import           System.Log.FastLogger (pushLogStr)
import           System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet, toLogStr)
import           Network.Wai.Handler.Warp    (Settings, defaultSettings,
                                              defaultShouldDisplayException,
                                              runSettings, setHost,
                                              setOnException, setPort, getPort)
import           Network.Wai.Middleware.RequestLogger (Destination (Logger),
                                                       IPAddrSource (..),
                                                       OutputFormat (..), destination,
                                                       mkRequestLogger, outputFormat)
import           Handler.Devices
import           Handler.Feedback
import           Handler.Health
import           Handler.Home
import           Handler.PlaybackGrant
import           Handler.PlaybackGrants
import           Handler.Subscribers

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and return a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    -- Some basic initializations: HTTP connection manager, logger
    appHttpManager <- newManager
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger

    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let mkFoundation appConnPool = App {..}
        -- The App {..} syntax is an example of record wild cards. For more
        -- information, see:
        -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation appLogger

    -- Now create a pool with the logging function
    pool <- flip runLoggingT logFunc $ createPostgresqlPool
            (pgConnStr  $ appDatabaseConf appSettings)
            (pgPoolSize $ appDatabaseConf appSettings)

    -- Perform database migrations using our application's logging settings.
    flip runLoggingT logFunc $ Migrations.main appSettings pool

    -- Return the foundation
    return $ mkFoundation pool

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applyng some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- makeLogWare foundation
    -- Create the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation
    return $ logWare $
      -- Disable gzip compression, nginx can handle this for us
      (acceptOverride . autohead . methodOverride . cors createCors) appPlain

makeLogWare :: App -> IO Middleware
makeLogWare foundation = mkRequestLogger def {
  outputFormat =
      if appDetailedRequestLogging $ appSettings foundation
      then Detailed True
      else Apache
           (if appIpFromHeader $ appSettings foundation
             then FromFallback
             else FromSocket)
  , destination = Logger $ loggerSet $ appLogger foundation
  }

-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
      setPort (appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setOnException (\_req e ->
        when (defaultShouldDisplayException e) $ messageLoggerSource
            foundation
            (appLogger foundation)
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app <- makeApplication foundation
    return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = do
#if DEVELOPMENT
    loadEnv
#endif
    loadYamlSettings [configSettingsYml] [] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    -- Get the settings from all relevant sources
    settings <- loadYamlSettingsArgs
        -- fall back to compile-time values, set to [] to require values at runtime
        [configSettingsYmlValue]
        -- allow environment variables to override
        useEnv
    -- Generate the foundation from the settings
    foundation <- makeFoundation settings
    -- Generate a WAI Application from the foundation
    app <- makeApplication foundation
    -- Fork a background process that periodically removes
    -- expired playback grants
    _ <- forkIO $ removeExpiredPlaybackGrants foundation
    -- Run the application with Warp
    runSettings (warpSettings foundation) app

removeExpiredPlaybackGrants :: App -> IO ()
removeExpiredPlaybackGrants app = do
  now <- getCurrentTime
  pushLogStr (loggerSet . appLogger $ app) $ toLogStr ("Removing expired playback grants" :: ByteString)
  _ <- runSqlPool (deleteWhere [ PlaybackGrantExpires >. now ]) (appConnPool app)
  -- Every 30 minutes
  _ <- threadDelay $ 1000000 * 60 * 30
  removeExpiredPlaybackGrants app

--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app1 <- makeApplication foundation
    return (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()

---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------
-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

-- | Run DB queries
db :: ReaderT SqlBackend (HandlerT App IO) a -> IO a
db = handler . runDB
