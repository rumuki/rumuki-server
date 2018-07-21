module Handler.DeviceUpdate (postDeviceUpdateR) where

import           Data.Aeson
import           Data.Time.Clock      (getCurrentTime)
import           Import
import           Model.PerpetualGrant ()
import           Model.PlaybackGrant  ()
import           Model.RemoteTransfer (RemoteTransferView (..),
                                       remoteTransferObjectURL,
                                       remoteTransferPublicURL)

data POSTRequest = POSTRequest [Text] ByteString

instance FromJSON POSTRequest where
  parseJSON = withObject "device update request" $ \o ->
    POSTRequest
    <$> o .: "recordingUIDs"
    <*> o .: "deviceKeyFingerprint"

postDeviceUpdateR :: Handler Value
postDeviceUpdateR = do
  settings <- appSettings <$> getYesod
  now <- liftIO getCurrentTime
  POSTRequest uids' keyFingerprint <- requireJsonBody

  -- Update the last updated value on the device:
  maybeDevice <- runDB $ selectFirst [ DeviceKeyFingerprint ==. keyFingerprint ] []
  case maybeDevice of
    Just (Entity did _) -> runDB $ update did [DeviceUpdated =. Just now]
    _ -> return ()

  transfers' <- runDB $ selectList [ RemoteTransferRecipientKeyFingerprint ==. keyFingerprint
                                   , RemoteTransferSeen ==. Nothing ] []
  transfers <- filterExistingTransfers transfers'

  -- Add the new remote transfer UIDs into the uids list
  let uids = uids' ++ map (remoteTransferRecordingUID . entityVal) transfers

  detections     <- runDB $ selectList [ScreenCaptureDetectionAffectedDeviceKeyFingerprint ==. keyFingerprint] []
  playbackGrants <- runDB $ selectList [PlaybackGrantRecordingUID <-. uids, PlaybackGrantExpires >. now] []
  perpetualGrants <- runDB $ selectList [PerpetualGrantRecordingUID <-. uids, PerpetualGrantExpires >. now] []

  -- Update the seen property of each of the remote transfers:
  traverse_
    (\t -> runDB $ update (entityKey t) [RemoteTransferSeen =. Just now])
    (filter (isNothing . remoteTransferSeen . entityVal) transfers)

  transferViews <- forM transfers $ \(Entity _ t) -> do
    let url = remoteTransferPublicURL t settings
    return $ RemoteTransferView t url

  sendResponseStatus status200 $ object [
      "playbackGrants"          .= playbackGrants
    , "perpetualGrants"         .= perpetualGrants
    , "screenCaptureDetections" .= detections
    , "remoteTransfers"         .= transferViews ]

-- | Given a list of remote transfers, filters out any transfers that either
-- no longer exist, or are incompletely uploaded.
filterExistingTransfers :: [Entity RemoteTransfer] -> Handler [Entity RemoteTransfer]
filterExistingTransfers transfers = do
  settings <- appSettings <$> getYesod
  httpClient <- appHttpClient <$> getYesod
  gcAuthorizer <- appGoogleCloudAuthorizer <$> getYesod
  fmap catMaybes $ sequenceA $ flip fmap transfers $ \transfer@(Entity _ t) -> do
    let objectURL = remoteTransferObjectURL t settings
    request <- parseRequest ("GET " ++ objectURL) >>= liftIO . gcAuthorizer
    response <- liftIO $ httpClient request
    bool (return Nothing) (return $ Just transfer) $ responseStatus response == status200
