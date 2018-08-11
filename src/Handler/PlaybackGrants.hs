module Handler.PlaybackGrants where

import           Data.Aeson
import           Data.Time.Clock
import           Import
import           Model.Device
import           Model.PlaybackGrant    ()
import           Model.PushNotification

data POSTRequest =
  POSTRequest { postRequestKeyCipher :: ByteString
              , postRequestRecipientKeyFingerprint :: ByteString
              , postRequestKeyOffset :: Maybe Int
              }

instance FromJSON POSTRequest where
  parseJSON = withObject "playback grants request" $ \o ->
    POSTRequest
    <$> o .: "keyCipher"
    <*> o .: "recipientKeyFingerprint"
    <*> o .:? "keyOffset"

postPlaybackGrantsR :: Text -> Handler Value
postPlaybackGrantsR recordingUID = do
  req <- requireJsonBody
  now <- liftIO getCurrentTime
  let expiry = addUTCTime (60 * 60 * 24 * 7) now
  let pg = PlaybackGrant
           recordingUID
           (postRequestRecipientKeyFingerprint req)
           (postRequestKeyCipher req)
           (postRequestKeyOffset req)
           expiry
           now

  (mpgid, deviceUnseenCounts) <- runDB $ do
    mpgid' <- insertUnique pg
    deviceUnseenCounts' <- buildDeviceUnseenCounts $ postRequestRecipientKeyFingerprint req
    return (mpgid', deviceUnseenCounts')

  pgid <- maybe (error "Could not create playback grant") return mpgid

  sequence_ $ flip map deviceUnseenCounts $ \(device, unseenCount) ->
    forkAndSendPushNotificationI MsgNewPlaybackGrantReceived unseenCount device

  sendResponseStatus status201 $ object ["playbackGrant" .= Entity pgid pg]

getPlaybackGrantsR :: Text -> Handler Value
getPlaybackGrantsR recordingUid = do
  now <- liftIO getCurrentTime
  grants <- runDB $ selectList
            [ PlaybackGrantRecordingUID ==. recordingUid
            , PlaybackGrantExpires >. now ]
            []

  sendResponseStatus status200 $ object [ "playbackGrants" .= grants ]

newtype DELETERequest =
  DELETERequest { deleteRequestRecipientKeyFingerprint :: ByteString }

instance FromJSON DELETERequest where
  parseJSON = withObject "playback grants delete request" $ \o ->
    DELETERequest
    <$> o .: "recipientKeyFingerprint"

deletePlaybackGrantsR :: Text -> Handler Value
deletePlaybackGrantsR recordingUid = do
  jsonResult <- parseJsonBody
  runDB $ deleteWhere $
    case jsonResult of
      (Success req) ->
        [ PlaybackGrantRecordingUID ==. recordingUid
        , PlaybackGrantRecipientKeyFingerprint ==. deleteRequestRecipientKeyFingerprint req ]
      (Error _) ->
        [ PlaybackGrantRecordingUID ==. recordingUid ]
  sendResponseStatus status204 ()
