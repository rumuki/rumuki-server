module Handler.MarkAsSeen where

import           Data.Aeson
import           Import

data POSTRequest = POSTRequest { recordingUIDs :: [Text]
                               , recipientKeyFingerprint :: ByteString }

instance FromJSON POSTRequest where
  parseJSON = withObject "post mark as seen request" $ \o ->
    POSTRequest
    <$> o .: "recordingUIDs"
    <*> o .: "deviceKeyFingerprint"

-- For the given recording UID, mark all associated remote transfers as seen.
postMarkAsSeenR :: Handler Value
postMarkAsSeenR = do
  now <- liftIO getCurrentTime
  req <- requireJsonBody
  let ruids = recordingUIDs req
  let keyFingerprint = recipientKeyFingerprint req
  runDB $ updateWhere
    [ RemoteTransferSeen ==. Nothing
    , RemoteTransferRecordingUID <-. ruids
    , RemoteTransferRecipientKeyFingerprint ==. keyFingerprint ]
    [ RemoteTransferSeen =. Just now ]

  sendResponseStatus status200 ()
