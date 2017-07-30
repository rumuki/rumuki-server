module Handler.Devices where

import           Data.Aeson        (withObject, (.:?))
import           Data.Time.Clock   (getCurrentTime)
import           Handler.Extension
import           Import

data CreateDeviceRequest = CreateDeviceRequest
  Text
  ByteString
  (Maybe ByteString)
  (Maybe Text)

instance FromJSON CreateDeviceRequest where
  parseJSON = withObject "device request" $
    \o -> CreateDeviceRequest
          <$> o .:  "deviceToken"
          <*> o .:  "keyFingerprint"
          <*> o .:? "apnDeviceToken"
          <*> o .:? "preferredLocalization"

postDevicesR :: Handler Value
postDevicesR = do
  now <- liftIO getCurrentTime
  CreateDeviceRequest
    deviceToken
    keyFingerprint
    mApnToken
    mPreferredLocalization <- requireJsonBody
  _ <- runDB $ upsertBy
    (UniqueDeviceToken deviceToken)
    (Device deviceToken keyFingerprint mApnToken (Just now) mPreferredLocalization) []
  sendResponseStatus status201 emptyResponse

data DeleteDeviceRequest = DeleteDeviceRequest Text [Text]

instance FromJSON DeleteDeviceRequest where
  parseJSON = withObject "delete device request" $
    \o -> DeleteDeviceRequest
          <$> o .: "deviceToken"
          <*> o .: "recordingUIDs"

deleteDevicesR :: Handler Value
deleteDevicesR = do
  DeleteDeviceRequest deviceToken recordingUIDs <- requireJsonBody
  _ <- sequence $ map (runDB . deleteWhere . (:[]) . (==.) PlaybackGrantRecordingUID) recordingUIDs
  runDB $ deleteBy $ UniqueDeviceToken deviceToken
  sendResponseStatus status204 ()
