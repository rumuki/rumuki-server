module TestFactory ( factoryDevice
                   , factoryPlaybackGrant
                   , factoryRemoteTransfer
                   , factoryPerpetualGrant ) where

import           ClassyPrelude
import           Data.Time.Clock
import           Database.Persist
import           Database.Persist.Sql (SqlBackend)
import           Model
import           System.Entropy

factoryDevice :: (MonadIO m, SqlBackend ~ backend)
               => (Device -> Device) -- ^ Chance to manipulate the device
               -> ReaderT backend m (Maybe (Entity Device))

factoryDevice transform = do
  now <- liftIO getCurrentTime
  let device = transform Device
        { deviceToken    = "abcd123-123123123-123123123-123123123"
        , deviceApnToken = Just "740f4707bebcf74f9b7c25d48e3358945f6aa01da5ddb387462c7eaf61bb78ad"
        , deviceKeyFingerprint = "ABCD1234ABCD1234ABCD1234ABCD1234ABCD1234ABCD1234ABCD1234ABCD1234"
        , deviceUpdated = Just now
        , devicePreferredLocalization = Just "en" }
  uid <- insert device
  return $ Just $ Entity uid device

factoryPlaybackGrant :: (MonadIO m, SqlBackend ~ backend)
                     => Entity Device -- ^ Recipient
                     -> (PlaybackGrant -> PlaybackGrant)
                     -> ReaderT backend m (Entity PlaybackGrant)

factoryPlaybackGrant (Entity _ rd) transform = do
  key <- liftIO $ getEntropy 32
  now <- liftIO getCurrentTime
  let expires = addUTCTime (60 * 60 * 24) now
  let playbackGrant = transform PlaybackGrant { playbackGrantRecordingUID = "recording123"
                                              , playbackGrantRecipientKeyFingerprint = deviceKeyFingerprint rd
                                              , playbackGrantKeyCipher = key
                                              , playbackGrantKeyOffset = Nothing
                                              , playbackGrantExpires = expires
                                              , playbackGrantCreated = now
                                              }
  pgid <- insert playbackGrant
  return $ Entity pgid playbackGrant

factoryPerpetualGrant :: (MonadIO m, SqlBackend ~ backend)
                      => Entity Device -- ^ Recipient
                      -> (PerpetualGrant -> PerpetualGrant)
                      -> ReaderT backend m (Entity PerpetualGrant)

factoryPerpetualGrant (Entity _ rd) transform = do
  key <- liftIO $ getEntropy 32
  now <- liftIO getCurrentTime
  let expires = addUTCTime (60 * 60 * 24) now
  let perpetualGrant = transform PerpetualGrant
        { perpetualGrantRecordingUID = "recording123"
        , perpetualGrantRecipientKeyFingerprint = deviceKeyFingerprint rd
        , perpetualGrantKeyCipher = key
        , perpetualGrantExpires = expires
        , perpetualGrantCreated = now
        }
  pgid <- insert perpetualGrant
  return $ Entity pgid perpetualGrant

factoryRemoteTransfer :: (MonadIO m, SqlBackend ~ backend)
                            => (RemoteTransfer -> RemoteTransfer)
                            -> ReaderT backend m (Entity RemoteTransfer)
factoryRemoteTransfer transform = do
  now <- liftIO getCurrentTime
  let transfer = transform $ RemoteTransfer "recording123" "" "" "" "" "" Nothing now
  tid <- insert transfer
  return $ Entity tid transfer
