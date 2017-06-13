module Handler.DeviceUpdate (postDeviceUpdateR) where

import           Data.Aeson
import           Data.Time.Clock      (getCurrentTime)
import           Import
import           Model.PlaybackGrant  ()
import           Model.RemoteTransfer (remoteTransferObjectURL)

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
  transfers' <- runDB $ selectList [ RemoteTransferRecipientKeyFingerprint ==. keyFingerprint ] []
  transfers <- filterExistingTransfers . fmap entityVal $ transfers'

  sendResponseStatus status200 $ object [ "playbackGrants"          .= grants
                                        , "screenCaptureDetections" .= detections
                                        , "remoteTransfers"   .= transfers ]

-- | Given a list of long distance transfers, filters out any transfers that either
-- no longer exist, or are incompletely uploaded.
filterExistingTransfers :: [RemoteTransfer] -> Handler [RemoteTransfer]
filterExistingTransfers transfers = do
  httpClient <- appHttpClient <$> getYesod
  gcAuthorizer <- appGoogleCloudAuthorizer <$> getYesod
  fmap catMaybes $ sequenceA $ flip fmap transfers $ \t -> do
    objectURL <- remoteTransferObjectURL t
    request <- parseRequest ("GET " ++ objectURL) >>= liftIO . gcAuthorizer
    response <- liftIO $ httpClient request
    bool (return Nothing) (return $ Just t) $ responseStatus response == status200
