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
  devices <- runDB $ selectList [DeviceKeyFingerprint ==. keyFingerprint] []
  _ <- sequence $ (flip map) devices $ \(Entity _ device) -> do
    let kf = deviceKeyFingerprint device
    _ <- runDB $ insertUnique (ScreenCaptureDetection kf recordingUID)
    outstandingGrantsCount <- countUnseenPlaybackGrants device
    outstandingRecordingsCount <- countUnseenRecordings device
    forkAndSendPushNotificationI
      MsgScreenCaptureDetected
      (max 1 (outstandingGrantsCount + outstandingRecordingsCount))
      device
  sendResponseStatus status201 emptyResponse
