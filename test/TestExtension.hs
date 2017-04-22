{-# LANGUAGE QuasiQuotes #-}

module TestExtension where

import           Application                  (makeFoundation, makeLogWare)
import           ClassyPrelude
import           Data.Aeson                   (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy         as LB
import           Database.Persist             hiding (get)
import           Database.Persist.Sql         (SqlBackend, SqlPersistM,
                                               connEscapeName, rawExecute,
                                               rawSql, runSqlPersistMPool,
                                               unSingle)
import           Foundation
import qualified Network.HTTP.Client.Internal as HTTP (Request (..),
                                                       Response (..),
                                                       ResponseClose (..),
                                                       createCookieJar)
import qualified Network.HTTP.Simple          as HTTP (getRequestHeader)
import qualified Network.HTTP.Types           as HTTP
import           Network.Wai.Test             as W
import           Test.Hspec
import           Test.HUnit                   (assertBool)
import           Text.Shakespeare.Text        (st)
import           Yesod.Default.Config2        (ignoreEnv, loadYamlSettings)
import           Yesod.Test                   as Y

-- | This method unwraps the maybe results generated by the
--   the factories.
retrieve :: (MonadIO m)
         => ReaderT backend m (Maybe (Entity a))
         -> ReaderT backend m (Entity a)

retrieve = (=<<) $ maybe (fail "Could not get entity") return

responseEquals :: (FromJSON a, Eq a) => String -> a -> YesodExample site ()
responseEquals t v = responseSatisfies t (v==)

responseSatisfies :: (FromJSON a) => String -> (a -> Bool) -> YesodExample site ()
responseSatisfies explanation p = withResponse $ \res ->
    liftIO $ assertBool explanation $
           case eitherDecode $ simpleBody res of
             Right v -> p v
             Left _ -> False

boolIsTrue :: String -> Bool -> YesodExample site ()
boolIsTrue s b = liftIO $ assertBool s b

boolIsFalse :: String -> Bool -> YesodExample site ()
boolIsFalse s b = liftIO $ assertBool s (not b)

requestJSON :: RequestBuilder App () -> YesodExample App ()
requestJSON b = Y.request $ addJSONHeader >> b

addJSONHeader :: RequestBuilder App ()
addJSONHeader = addRequestHeader (HTTP.hAccept, "application/json")

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

runDBWithApp :: App -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query (appConnPool app)

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
    settings <- loadYamlSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        ignoreEnv
    app <- makeFoundation settings
    logWare <- liftIO $ makeLogWare app
    wipeDB app
    return (app, logWare)

withAppAndMockResponder :: (HTTP.Request -> HTTP.Response LB.ByteString) -> SpecWith (TestApp App) -> Spec
withAppAndMockResponder responder =  before $ do
  settings <- loadYamlSettings
              ["config/test-settings.yml", "config/settings.yml"]
              []
              ignoreEnv
  app' <- makeFoundation settings
  let app = app' { appHttpClient = return <$> responder }
  logWare <- liftIO $ makeLogWare app
  wipeDB app
  return (app, logWare)

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = runDBWithApp app $ do
    tables <- getTables
    sqlBackend <- ask

    let escapedTables = map (connEscapeName sqlBackend . DBName) tables
        query = "TRUNCATE TABLE " ++ intercalate ", " escapedTables
    rawExecute query []

getTables :: MonadIO m => ReaderT SqlBackend m [Text]
getTables = do
    tables <- rawSql [st|
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = 'public' AND
              table_name  != 'installed_migrations';
    |] []

    return $ map unSingle tables

mockGCResponder :: ByteString
                -> HTTP.Method
                -> (HTTP.Response LB.ByteString -> HTTP.Response LB.ByteString)
                -> HTTP.Request
                -> HTTP.Response LB.ByteString

mockGCResponder reqPath reqMethod transform req

  | foldr (&&) True [ HTTP.host req == "www.googleapis.com"
                    , HTTP.path req == reqPath
                    , HTTP.method req == reqMethod
                    , requestIsGCAuthorized req ] =
    transform HTTP.Response { HTTP.responseStatus    = HTTP.status200
                            , HTTP.responseVersion   = HTTP.http11
                            , HTTP.responseHeaders   = [("Content-Length", "1")]
                            , HTTP.responseCookieJar = HTTP.createCookieJar []
                            , HTTP.responseClose'    = HTTP.ResponseClose $ return ()
                            , HTTP.responseBody      = "{}" } -- In reality an object resource is returned
  | otherwise = error $ "Unexpected request: " ++ show req

requestIsGCAuthorized :: HTTP.Request -> Bool
requestIsGCAuthorized = (==) (Just "Bearer mock-gcs")
                        . headMay
                        . HTTP.getRequestHeader HTTP.hAuthorization
