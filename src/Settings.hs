{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import           ClassyPrelude.Yesod         hiding (throw)
import           Control.Exception           (throw)
import           Data.Aeson                  (Result (..), fromJSON, withObject,
                                              (.!=), (.:?))
import           Data.Extension              ()
import           Data.FileEmbed              (embedFile)
import           Data.Yaml                   (decodeEither')
import           Database.Persist.Postgresql (PostgresConf)
import           Network.Wai.Handler.Warp    (HostPreference)
import           Yesod.Default.Config2       (applyEnvValue, configSettingsYml)

-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.
data AppSettings = AppSettings
    { appDatabaseConf           :: PostgresConf
    -- ^ Configuration settings for accessing the database.
    , appRoot                   :: Text
    -- ^ Base for all generated URLs.
    , appHost                   :: HostPreference
    -- ^ Host/interface the server should bind to.
    , appPort                   :: Int
    -- ^ Port to listen on
    , appIpFromHeader           :: Bool
    -- ^ Get the IP address from the header when logging. Useful when sitting
    -- behind a reverse proxy.
    , appIsTesting              :: Bool
    -- ^ Set to true when running tests

    , appDetailedRequestLogging :: Bool
    -- ^ Use detailed request logging system
    , appShouldLogAll           :: Bool
    -- ^ Should all log messages be displayed?

    , appNotificationsHost      :: String
    , appNotificationsPort      :: Int

    , appGCSBucketName          :: String
    , appGCSAuthorize           :: Bool
    -- ^ Whether or not the app should authorize requests to GCS.
    -- This should be set to @False@ during testing.

    }

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        appDatabaseConf           <- o .: "database"
        appRoot                   <- o .: "app-root"
        appHost                   <- fromString <$> o .: "host"
        appPort                   <- o .: "port"
        appIpFromHeader           <- o .: "ip-from-header"

        appIsTesting              <- o .:? "app-is-testing"   .!= False
        appDetailedRequestLogging <- o .:? "detailed-logging" .!= False
        appShouldLogAll           <- o .:? "should-log-all"   .!= False

        appNotificationsHost      <- o .: "notifications-host"
        appNotificationsPort      <- o .: "notifications-port"

        appGCSBucketName          <- o .: "gcs-bucket-name"
        appGCSAuthorize           <- o .:? "gcs-authorize" .!= True

        return AppSettings {..}

-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

-- | @config/settings.yml@, parsed to a @Value@.
configSettingsYmlValue :: Value
configSettingsYmlValue = either throw id $ decodeEither' configSettingsYmlBS

-- | A version of @AppSettings@ parsed at compile time from @config/settings.yml@.
compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
    case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
        Error e -> error e
        Success settings -> settings
