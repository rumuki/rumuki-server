module Handler.DeviceUpdate (getDeviceUpdateR) where

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

getDeviceUpdateR :: Handler Value
getDeviceUpdateR = do
  now <- liftIO getCurrentTime
  GETRequest uids keyFingerprint <- requireJsonBody
  detections <- runDB $ selectList [ ScreenCaptureDetectionAffectedDeviceKeyFingerprint ==. keyFingerprint ] []
  grants <- runDB $ selectList [ PlaybackGrantRecordingUID <-. uids , PlaybackGrantExpires >. now ] []
  sendResponseStatus status200 $ object [ "playbackGrants" .= grants
                                        , "screenCaptureDetections" .= detections ]
