{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Application
    ( getApplicationDev
    , appMain
    , develMain
    , makeFoundation
    , makeLogWare
    , getApplicationRepl
    , removeStaleObjects
    , shutdownApp
    , handler
    , db
    ) where

import           Blaze.ByteString.Builder              (toByteString,
                                                        toLazyByteString)
import           Control.Concurrent                    (forkIO, threadDelay)
import           Control.Monad.Logger                  (liftLoc, runLoggingT)
import           Data.Aeson                            (encode, object, toJSON,
                                                        (.=))
import           Data.Foldable                         (sequenceA_)
import           Data.Time.Clock
import           Database.Persist.Postgresql           (createPostgresqlPool,
                                                        pgConnStr, pgPoolSize)
import           Database.Persist.Sql
import           Handler.Devices
import           Handler.DeviceUpdate
import           Handler.Feedback
import           Handler.Health
import           Handler.Home
import           Handler.MarkAsSeen
import           Handler.PerpetualGrant
import           Handler.PerpetualGrants
import           Handler.PlaybackGrant
import           Handler.PlaybackGrants
import           Handler.RemoteTransfer
import           Handler.RemoteTransfers
import           Handler.ScreenCaptureDetections
import           Handler.Subscribers
import           Import
import           Language.Haskell.TH.Syntax            (qLocation)
import qualified Migrations
import qualified Network.Google                        as GC (LogLevel (..))
import qualified Network.Google.Auth                   as GC
import qualified Network.Google.Storage                as GCS (storageReadWriteScope)
import qualified Network.HTTP.Client                   as HTTP
import qualified Network.HTTP.Client.TLS               as HTTP (tlsManagerSettings)
import qualified Network.HTTP.Simple                   as HTTP (setRequestHeader)
import qualified Network.HTTP.Types                    as HTTP (hAuthorization)
import           Network.Wai                           hiding (responseStatus)
import           Network.Wai.Handler.Warp              (Settings,
                                                        defaultSettings,
                                                        defaultShouldDisplayException,
                                                        getPort, runSettings,
                                                        setHost, setOnException,
                                                        setPort)
import           Network.Wai.Middleware.AcceptOverride
import           Network.Wai.Middleware.Autohead
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.MethodOverride
import           Network.Wai.Middleware.RequestLogger
import           Prelude                               (read)
import           Text.Printf                           (printf)

import           Model.RemoteTransfer                  (remoteTransferObjectURL)

import           System.Log.FastLogger                 (defaultBufSize,
                                                        newStdoutLoggerSet,
                                                        pushLogStrLn, toLogStr)
import           Yesod.Core.Types                      (loggerSet)

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
    -- Some basic initialization: HTTP connection manager, logger
    appHttpManager <- newManager
    loggerSet <- newStdoutLoggerSet defaultBufSize
    appLogger <- makeYesodLogger loggerSet
    httpManager <- HTTP.newManager HTTP.tlsManagerSettings
    let appHttpClient = flip HTTP.httpLbs httpManager

    appGoogleCloudAuthorizer <-
      if appGCSAuthorize appSettings
      then do
        gcCredentials <- GC.allow GCS.storageReadWriteScope <$> GC.getApplicationDefault httpManager
        let gcLogger logLevel = case logLevel of
              GC.Error -> pushLogStrLn loggerSet . toLogStr . toByteString
              _ -> return . const ()
        gcStore <- GC.initStore gcCredentials gcLogger httpManager
        return $ \request -> GC.authorize request gcStore gcLogger httpManager
      else return $ return . HTTP.setRequestHeader HTTP.hAuthorization ["Bearer mock-gcs"]

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
makeLogWare foundation
  | appIsTesting (appSettings foundation) = return id
  | otherwise = mkRequestLogger def
    { outputFormat = CustomOutputFormatWithDetails formatAsJSON
    , destination = Logger $ loggerSet $ appLogger foundation }

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

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv

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
    -- Fork a background thread that periodically removes stale objects
    let runBackgroundThread = removeStaleObjects foundation >>
                              threadDelay (1000000 * 60 * 30) >>
                              runBackgroundThread
    _ <- forkIO runBackgroundThread
    -- Run the application with Warp
    runSettings (warpSettings foundation) app

-- | When run, this function will remove state that it considers stale from
-- either the database, or Google Cloud Storage. Things that it considers stale:
--
-- * Any expired playback grants
-- * Any seen remote transfers that are over 3 weeks old
removeStaleObjects :: App -> IO ()
removeStaleObjects app = runSqlPool executeRemoval (appConnPool app)
  where
    settings = appSettings app
    logMessage message = if appIsTesting settings
      then return ()
      else liftIO $ pushLogStrLn (loggerSet . appLogger $ app) $
           toLogStr (encode . object $ [ "message" .= (message :: Text) ])

    -- Returns True if the transfer object was deleted, or if it
    -- never existed in the first place.
    deleteRemoteTransferGCSObject (Entity _ t) = do
      request' <- parseRequest $ "DELETE " ++ remoteTransferObjectURL t settings
      request <- liftIO . appGoogleCloudAuthorizer app $ request'
      response <- liftIO . appHttpClient app $ request
      return $ elem (responseStatus response) [status204, status404]

    executeRemoval = do
      now <- liftIO getCurrentTime
      let threeWeeksAgo = addUTCTime (-1 * 21 * 24 * 60 * 60) now
      let thirtyDaysAgo = addUTCTime (-1 * 30 * 24 * 60 * 60) now

      logMessage "Removing expired playback grants"
      _ <- deleteWhere [ PlaybackGrantExpires <. now ]

      logMessage "Removing stale remote transfers"
      transfers <- selectList ([ RemoteTransferSeen !=. Nothing
                               , RemoteTransferSeen <. Just threeWeeksAgo ] ||.
                               [ RemoteTransferCreated <. thirtyDaysAgo ]) []
      sequenceA_ $ flip map transfers $ \e@(Entity k t) -> do
        isDeleteSuccessful <- deleteRemoteTransferGCSObject e
        if isDeleteSuccessful
          then delete k
          else logMessage ("Failed to delete GCS object for: " ++ remoteTransferRecordingUID t)

      return ()

--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
  (_, wsettings, app) <- getApplicationPrerequisites
  return (wsettings, app)

getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
  (foundation, wsettings, app) <- getApplicationPrerequisites
  return (getPort wsettings, foundation, app)

getApplicationPrerequisites :: IO (App, Settings, Application)
getApplicationPrerequisites = do
  settings <- getAppSettings
  foundation <- makeFoundation settings
  wsettings <- getDevSettings $ warpSettings foundation
  app <- makeApplication foundation
  return (foundation, wsettings, app)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()

---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------
-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

-- | Run DB queries
db :: ReaderT SqlBackend (HandlerFor App) a -> IO a
db = handler . runDB

------------------------
-- Functions for logging
------------------------
formatAsJSON :: OutputFormatterWithDetails
formatAsJSON date req status responseSize duration _ response =
  toLogStr (encode $
    object
      [ "message"            .= ("Route hit" :: Text)
      , "request_method"     .= decodeUtf8 (requestMethod req)
      , "request_host"       .= (decodeUtf8 <$> requestHeaderHost req)
      , "request_path"       .= decodeUtf8 (rawPathInfo req)
      , "request_size"       .= requestBodyLengthToJSON (requestBodyLength req)
      , "request_user_agent" .= (decodeUtf8 <$> requestHeaderUserAgent req)
      , "duration_ms"        .= (readAsDouble . printf "%.2f" . rationalToDouble $ toRational duration * 1000)
      , "response_status"    .= statusCode status
      , "response_size"      .= responseSize
      , "response_body"      .= if statusCode status >= 400
                                then Just . decodeUtf8 . toStrict . toLazyByteString $ response
                                else Nothing
      , "timestamp"          .= decodeUtf8 date
      ]) <> "\n"
  where
    requestBodyLengthToJSON :: RequestBodyLength -> Value
    requestBodyLengthToJSON ChunkedBody = String "Unknown"
    requestBodyLengthToJSON (KnownLength l) = toJSON l

    readAsDouble :: String -> Double
    readAsDouble = read

    rationalToDouble :: Rational -> Double
    rationalToDouble = fromRational
