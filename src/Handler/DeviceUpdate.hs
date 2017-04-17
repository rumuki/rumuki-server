module Handler.DeviceUpdate (postDeviceUpdateR) where

import           Data.Aeson
import           Data.Time.Clock            (getCurrentTime)
import           Import
import           Model.LongDistanceTransfer (longDistanceTransferObjectURL)
import           Model.PlaybackGrant        ()

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
  transfers' <- runDB $ selectList [ LongDistanceTransferRecipientKeyFingerprint ==. keyFingerprint ] []
  transfers <- filterExistingTransfers . fmap entityVal $ transfers'

  sendResponseStatus status200 $ object [ "playbackGrants"          .= grants
                                        , "screenCaptureDetections" .= detections
                                        , "longDistanceTransfers"   .= transfers ]

filterExistingTransfers :: [LongDistanceTransfer] -> Handler [LongDistanceTransfer]
filterExistingTransfers transfers = do
  httpClient <- appHttpClient <$> getYesod
  settings <- appSettings <$> getYesod
  fmap catMaybes $ sequence $ flip fmap transfers $ \t -> do
    objectURL <- longDistanceTransferObjectURL t
    request' <- parseRequest $ "GET " ++ objectURL
    let request = setQueryString [("key", Just . appGCSAPIKey $ settings)] request'
    response <- liftIO $ httpClient request
    bool (return Nothing) (return $ Just t) $ responseStatus response == status200
