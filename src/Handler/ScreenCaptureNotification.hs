module Handler.ScreenCaptureNotification (
  postScreenCaptureNotificationR) where

import           Data.Aeson
import           Data.Time.Clock        (getCurrentTime)
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
  now <- liftIO getCurrentTime
  (POSTRequest keyFingerprint recordingUID) <- requireJsonBody
  devices <- runDB $ selectList [DeviceKeyFingerprint ==. keyFingerprint] []
  _ <- sequence $ (flip map) devices $ \(Entity _ device) -> do
    outstandingGrantsCount <- runDB $ count [ PlaybackGrantExpires >. now
                                            , PlaybackGrantRecipientKeyFingerprint ==. deviceKeyFingerprint device ]
    forkAndSendPushNotificationI (MsgScreenCaptureDetected recordingUID) (max 1 outstandingGrantsCount) device
  sendResponseStatus status201 ()
