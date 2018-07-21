module Model.Device ( buildDeviceUnseenCounts
                    ) where

import           Data.Time.Clock
import           Import

buildDeviceUnseenCounts :: ByteString                  -- ^ Device key fingerprint
                        -> YesodDB App [(Device, Int)] -- ^ A list of devices and their unseen counts
buildDeviceUnseenCounts keyFingerprint = do
  now <- liftIO getCurrentTime
  devices <- selectList [DeviceKeyFingerprint ==. keyFingerprint] []
  sequence $ flip map devices $ \(Entity _ device) -> do
    outstandingGrantsCount <- countUnseenPlaybackGrants device now
    outstandingRecordingsCount <- countUnseenRecordings device
    newPerpetualGrantsCount <- countNewPerpetualGrants device now
    return (device, outstandingGrantsCount + outstandingRecordingsCount + newPerpetualGrantsCount)

countNewPerpetualGrants :: Device ->
                           UTCTime -> -- ^ Current time
                           YesodDB App Int
countNewPerpetualGrants device now = do
  let inferredLastAccess = fromMaybe (fallbackLastAccessfromNow now) $ deviceUpdated device
  count [ PerpetualGrantExpires >. now
        , PerpetualGrantRecipientKeyFingerprint ==. deviceKeyFingerprint device
        , PerpetualGrantCreated >. inferredLastAccess ]

countUnseenPlaybackGrants :: Device ->
                             UTCTime -> -- ^ Current time
                             YesodDB App Int
countUnseenPlaybackGrants device now = do
  let inferredLastAccess = fromMaybe (fallbackLastAccessfromNow now) $ deviceUpdated device
  count [ PlaybackGrantExpires >. now
        , PlaybackGrantRecipientKeyFingerprint ==. deviceKeyFingerprint device
        , PlaybackGrantCreated >. inferredLastAccess ]

countUnseenRecordings :: Device -> YesodDB App Int
countUnseenRecordings device =
  count [ RemoteTransferSeen ==. Nothing
        , RemoteTransferRecipientKeyFingerprint ==. deviceKeyFingerprint device ]

fallbackLastAccessfromNow :: UTCTime -> UTCTime
fallbackLastAccessfromNow = addUTCTime (-1 * 60 * 60 * 24 * 30)
