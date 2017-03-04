module Handler.ScreenCaptureNotification (
  postScreenCaptureNotificationR) where

import           Data.Aeson
import           Import
import           Model.PushNotification

data POSTRequest = POSTRequest ByteString Text

instance FromJSON POSTRequest where
  parseJSON = withObject "post screen capture notification request" $ \o ->
    POSTRequest
    <$> o .: "recipientKeyFingerprint"
    <*> o .: "recordingUID"

postScreenCaptureNotificationR :: Handler Value
postScreenCaptureNotificationR = do
  (POSTRequest keyFingerprint recordingUID) <- requireJsonBody
  devices <- runDB $ selectList [DeviceKeyFingerprint ==. keyFingerprint] []
  _ <- sequence $ map (forkAndSendPushNotificationI
                      (MsgScreenCaptureDetected recordingUID) .
                      entityVal) devices
  sendResponseStatus status201 ()
