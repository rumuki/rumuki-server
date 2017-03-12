module Handler.DeviceUpdate (postDeviceUpdateR) where

import           Data.Aeson
import           Data.Time.Clock     (getCurrentTime)
import           Import
import           Model.PlaybackGrant ()

data GETRequest = GETRequest [Text] ByteString

instance FromJSON GETRequest where
  parseJSON = withObject "device update request" $ \o ->
    GETRequest
    <$> o .: "recordingUIDs"
    <*> o .: "deviceKeyFingerprint"

postDeviceUpdateR :: Handler Value
postDeviceUpdateR = do
  now <- liftIO getCurrentTime
  GETRequest uids keyFingerprint <- requireJsonBody

  -- Update the last updated value on the device:
  maybeDevice <- runDB $ selectFirst [ DeviceKeyFingerprint ==. keyFingerprint ] []
  case maybeDevice of
    Just (Entity did _) -> runDB $ update did [DeviceUpdated =. Just now]
    _ -> return ()

  detections <- runDB $ selectList [ ScreenCaptureDetectionAffectedDeviceKeyFingerprint ==. keyFingerprint ] []
  grants <- runDB $ selectList [ PlaybackGrantRecordingUID <-. uids , PlaybackGrantExpires >. now ] []
  sendResponseStatus status200 $ object [ "playbackGrants" .= grants
                                        , "screenCaptureDetections" .= detections ]
