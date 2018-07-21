module Handler.ScreenCaptureDetections (
  postScreenCaptureDetectionsR) where

import           Data.Aeson
import           Handler.Extension
import           Import
import           Model.Device
import           Model.PushNotification

data POSTRequest = POSTRequest ByteString Text

instance FromJSON POSTRequest where
  parseJSON = withObject "post screen capture notification request" $ \o ->
    POSTRequest
    <$> o .: "recipientKeyFingerprint"
    <*> o .: "recordingUID"

postScreenCaptureDetectionsR :: Handler Value
postScreenCaptureDetectionsR = do
  (POSTRequest keyFingerprint recordingUID) <- requireJsonBody

  (detection', devicesAndUnseenCounts') <- runDB $ do
    detection <- insertUnique (ScreenCaptureDetection keyFingerprint recordingUID)
    devicesAndUnseenCounts <- buildDeviceUnseenCounts keyFingerprint
    return (detection, devicesAndUnseenCounts)

  when (isNothing detection')
    $ $(logWarn) "Failed to insert screen capture detection, it probably already exists?"

  sequence_ $ flip map devicesAndUnseenCounts' $ \(device, unseenCount) ->
    forkAndSendPushNotificationI MsgScreenCaptureDetected (max 1 unseenCount) device

  sendResponseStatus status201 emptyResponse
