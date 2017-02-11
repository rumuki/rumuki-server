module TestFactory ( factoryDevice
                   , factoryPlaybackGrant ) where

import ClassyPrelude
import System.Entropy
import Database.Persist
import Model
import Data.Time.Clock
import Database.Persist.Sql (SqlBackend)

factoryDevice :: (MonadIO m, SqlBackend ~ backend)
               => (Device -> Device) -- ^ Chance to manipulate the device
               -> ReaderT backend m (Maybe (Entity Device))

factoryDevice transform = do
  let device = transform Device { deviceToken    = "abcd123-123123123-123123123-123123123"
                                , deviceApnToken = Just "740f4707bebcf74f9b7c25d48e3358945f6aa01da5ddb387462c7eaf61bb78ad"
                                , deviceKeyFingerprint = "ABCD1234ABCD1234ABCD1234ABCD1234ABCD1234ABCD1234ABCD1234ABCD1234" }
  uid <- insert device
  return $ Just $ Entity uid device

factoryPlaybackGrant :: (MonadIO m, SqlBackend ~ backend)
                        => Entity Device -- ^ Recipient
                        -> (PlaybackGrant -> PlaybackGrant)
                        -> ReaderT backend m (Entity PlaybackGrant)

factoryPlaybackGrant (Entity _ rd) transform = do
  key <- liftIO $ getEntropy 32
  expires <- liftIO $ addUTCTime (60 * 60 * 24) <$> getCurrentTime
  let playbackGrant = transform PlaybackGrant { playbackGrantRecordingUID = "recording123"
                                              , playbackGrantRecipientKeyFingerprint = deviceKeyFingerprint rd
                                              , playbackGrantKeyCipher = key
                                              , playbackGrantKeyOffset = Nothing
                                              , playbackGrantExpires = expires
                                              }
  pgid <- insert playbackGrant
  return $ Entity pgid playbackGrant
