module Handler.DeviceUpdate (postDeviceUpdateR) where

import           Data.Aeson
import           Data.Time.Clock      (getCurrentTime)
import           Import
import           Model.PlaybackGrant  ()
import           Model.RemoteTransfer (RemoteTransferView (..),
                                       remoteTransferObjectURL,
                                       remoteTransferPublicURL)

data GETRequest = GETRequest [Text] ByteString

instance FromJSON GETRequest where
  parseJSON = withObject "device update request" $ \o ->
    GETRequest
    <$> o .: "recordingUIDs"
    <*> o .: "deviceKeyFingerprint"

postDeviceUpdateR :: Handler Value
postDeviceUpdateR = do
  settings <- appSettings <$> getYesod
  now <- liftIO getCurrentTime
  GETRequest uids' keyFingerprint <- requireJsonBody

  -- Update the last updated value on the device:
  maybeDevice <- runDB $ selectFirst [ DeviceKeyFingerprint ==. keyFingerprint ] []
  case maybeDevice of
    Just (Entity did _) -> runDB $ update did [DeviceUpdated =. Just now]
    _ -> return ()

  transfers'  <- runDB $ selectList [ RemoteTransferRecipientKeyFingerprint ==. keyFingerprint
                                    , RemoteTransferSeen ==. Nothing ] []
  transfers <- filterExistingTransfers . fmap entityVal $ transfers'

  -- Add the new remote transfer UIDs into the uids list
  let uids = uids' ++ map remoteTransferRecordingUID transfers

  detections  <- runDB $ selectList [ ScreenCaptureDetectionAffectedDeviceKeyFingerprint ==. keyFingerprint ] []
  grants      <- runDB $ selectList [ PlaybackGrantRecordingUID <-. uids , PlaybackGrantExpires >. now ] []
  -- Update the seen property of each of the remote transfers:
  _ <- sequenceA
       $ map (\t -> runDB $ update (entityKey t) [RemoteTransferSeen =. Just now])
       $ filter (isNothing . remoteTransferSeen . entityVal) transfers'

  transferViews <- (flip mapM) transfers $ \t -> do
    let url = remoteTransferPublicURL t settings
    return $ RemoteTransferView t url

  sendResponseStatus status200 $ object [
      "playbackGrants"          .= grants
    , "screenCaptureDetections" .= detections
    , "remoteTransfers"         .= transferViews ]

-- | Given a list of remote transfers, filters out any transfers that either
-- no longer exist, or are incompletely uploaded.
filterExistingTransfers :: [RemoteTransfer] -> Handler [RemoteTransfer]
filterExistingTransfers transfers = do
  settings <- appSettings <$> getYesod
  httpClient <- appHttpClient <$> getYesod
  gcAuthorizer <- appGoogleCloudAuthorizer <$> getYesod
  fmap catMaybes $ sequenceA $ flip fmap transfers $ \t -> do
    let objectURL = remoteTransferObjectURL t settings
    request <- parseRequest ("GET " ++ objectURL) >>= liftIO . gcAuthorizer
    response <- liftIO $ httpClient request
    bool (return Nothing) (return $ Just t) $ responseStatus response == status200
