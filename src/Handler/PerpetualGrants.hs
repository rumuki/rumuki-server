module Handler.PerpetualGrants where

import           Data.Aeson
import           Data.Time.Clock
import           Import
import           Model.Device
import           Model.PerpetualGrant   ()
import           Model.PushNotification

data POSTRequest =
  POSTRequest { postRequestKeyCipher :: ByteString
              , postRequestRecipientKeyFingerprint :: ByteString
              }

instance FromJSON POSTRequest where
  parseJSON = withObject "perpetual grants post request" $ \o ->
    POSTRequest
    <$> o .: "keyCipher"
    <*> o .: "recipientKeyFingerprint"

postPerpetualGrantsR :: Text -> Handler Value
postPerpetualGrantsR recordingUID = do
  req <- requireJsonBody
  now <- liftIO getCurrentTime
  let kf = postRequestRecipientKeyFingerprint req
  let expiry = addUTCTime (60 * 60 * 24 * 180) now
  let pg = PerpetualGrant
           recordingUID
           kf
           (postRequestKeyCipher req)
           expiry
           now

  -- Insert, or just update the expiry
  (Entity pgid pg', deviceUnseenCounts) <- runDB $ do
    newGrant <- upsert pg [PerpetualGrantExpires =. expiry]
    deviceUnseenCounts' <- buildDeviceUnseenCounts kf
    return (newGrant, deviceUnseenCounts')

  sequence_ $ flip map deviceUnseenCounts $ \(device, unseenCount) ->
    forkAndSendPushNotificationI MsgNewPerpetualGrantReceived unseenCount device

  sendResponseStatus status201 $ object ["perpetualGrant" .= Entity pgid pg']

getPerpetualGrantsR :: Text -> Handler Value
getPerpetualGrantsR recordingUid = do
  now <- liftIO getCurrentTime
  grants <- runDB $ selectList [ PerpetualGrantRecordingUID ==. recordingUid
                               , PerpetualGrantExpires >. now ] []
  sendResponseStatus status200 $ object [ "perpetualGrants" .= grants ]

data DELETERequest =
  DELETERequest { deleteRequestRecipientKeyFingerprint :: ByteString
                }

instance FromJSON DELETERequest where
  parseJSON = withObject "perpetual grants delete request" $ \o ->
    DELETERequest
    <$> o .: "recipientKeyFingerprint"

deletePerpetualGrantsR :: Text -> Handler Value
deletePerpetualGrantsR recordingUid = do
  req <- requireJsonBody :: Handler DELETERequest
  let kf = deleteRequestRecipientKeyFingerprint req
  runDB $ deleteWhere
    [ PerpetualGrantRecordingUID ==. recordingUid
    , PerpetualGrantRecipientKeyFingerprint ==. kf ]
  sendResponseStatus status204 ()
