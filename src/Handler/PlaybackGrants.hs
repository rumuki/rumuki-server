module Handler.PlaybackGrants where

import           Data.Aeson
import           Data.Time.Clock
import           Import
import           Model.PlaybackGrant    ()
import           Model.PushNotification

data PlaybackGrantsRequest =
  PlaybackGrantsRequest { keyCipher :: ByteString
                        , recipientKeyFingerprint :: ByteString
                        , keyOffset :: Maybe Int
                        }

instance FromJSON PlaybackGrantsRequest where
  parseJSON = withObject "playback grants request" $ \o ->
    PlaybackGrantsRequest
    <$> o .: "keyCipher"
    <*> o .: "recipientKeyFingerprint"
    <*> o .:? "keyOffset"

postPlaybackGrantsR :: Text -> Handler Value
postPlaybackGrantsR recordingUID = do
  req <- requireJsonBody
  now <- liftIO getCurrentTime
  let expiry = addUTCTime (60 * 60 * 24 * 7) now
  let pg = PlaybackGrant recordingUID
           (recipientKeyFingerprint req)
           (keyCipher req)
           (keyOffset req)
           expiry

  -- Try and insert
  pgid <- fromMaybeM (error "Could not create playback grant") $ runDB $ insertUnique pg

  -- If a device for the given fingerprint exists, fire a push notification
  devices <- runDB $ selectList [DeviceKeyFingerprint ==. recipientKeyFingerprint req] []
  _ <- sequence $ (flip map) devices $ \(Entity _ device) -> do
    outstandingGrantsCount <- runDB $ count [ PlaybackGrantExpires >. now
                                            , PlaybackGrantRecipientKeyFingerprint ==. deviceKeyFingerprint device ]
    forkAndSendPushNotificationI MsgNewPlaybackGrantReceived outstandingGrantsCount device

  sendResponseStatus status201 $ object ["playbackGrant" .= (ResponseView (Entity pgid pg))]

getPlaybackGrantsR :: Text -> Handler Value
getPlaybackGrantsR recordingUid = do
  now <- liftIO getCurrentTime
  grants <- runDB $ selectList
            [ PlaybackGrantRecordingUID ==. recordingUid
            , PlaybackGrantExpires >. now ]
            []

  sendResponseStatus status200 $ object [
    "playbackGrants" .= map ResponseView grants ]

deletePlaybackGrantsR :: Text -> Handler Value
deletePlaybackGrantsR recordingUid = do
  runDB $ deleteWhere [ PlaybackGrantRecordingUID ==. recordingUid ]
  sendResponseStatus status204 ()
