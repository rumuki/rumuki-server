module Handler.RemoteTransfers where

import           Data.Aeson             (withObject)
import           Data.Time.Clock
import           Import
import           Model.Device
import           Model.PushNotification (forkAndSendPushNotificationI)
import           Network.HTTP.Simple    (getResponseHeader)

data POSTRemoteTransfersRequest =
  POSTRemoteTransfersRequest { recordingUID :: Text
                             , recipientKeyFingerprint :: ByteString
                             , recordingNameCipher :: ByteString
                             , senderPublicKeyCipher :: ByteString
                             , senderNicknameCipher :: ByteString
                             , keyCipher :: ByteString
                             }

instance FromJSON POSTRemoteTransfersRequest where
  parseJSON = withObject "transfer request object" $ \o ->
    POSTRemoteTransfersRequest
    <$> o .: "recordingUID"
    <*> o .: "recipientKeyFingerprint"
    <*> o .: "recordingNameCipher"
    <*> o .: "senderPublicKeyCipher"
    <*> o .: "senderNicknameCipher"
    <*> o .: "keyCipher"

postRemoteTransfersR :: Handler Value
postRemoteTransfersR = do
  app <- getYesod
  req <- requireJsonBody
  now <- liftIO getCurrentTime
  let settings = appSettings app

  existingTransfer <- runDB $ getBy $ UniqueRemoteTransfer (recordingUID req)
  when (isJust existingTransfer) $ invalidArgsI [MsgTransferForRecordingAlreadyExists]

  request' <- parseRequest
              $ "POST https://www.googleapis.com/upload/storage/v1/b/"
              ++ appGCSBucketName settings
              ++ "/o"

  let zeroRedirectCount r = r { redirectCount = 0 }
  request <- liftIO
             $ appGoogleCloudAuthorizer app
             $ setQueryString [ ("uploadType", Just "resumable")
                              , ("name", Just . encodeUtf8 $ recordingUID req) ]
             $ zeroRedirectCount request'

  response <- liftIO . appHttpClient app $ request

  when (responseStatus response /= status200) $ error "could not create upload url"

  -- The upload URL is available in the @Location@ header,
  -- and is valid for up to 7 days.
  location <- maybe (error "could not create upload url") return
              $ headMay
              $ getResponseHeader "Location" response


  (maybeTransferID, deviceUnseenCounts) <- runDB $ do
    maybeTransferID' <- insertUnique $ RemoteTransfer
                        (recordingUID req)
                        (recipientKeyFingerprint req)
                        (recordingNameCipher req)
                        (senderPublicKeyCipher req)
                        (senderNicknameCipher req)
                        (keyCipher req)
                        Nothing
                        Video
                        now
    deviceUnseenCounts' <- buildDeviceUnseenCounts $ recipientKeyFingerprint req
    return (maybeTransferID', deviceUnseenCounts')

  when (isNothing maybeTransferID) (error "Could not create the transfer: DB error")

  -- Fire a push notification(s)
  sequence_ $ flip map deviceUnseenCounts $ \(device, unseenCount) ->
    forkAndSendPushNotificationI MsgNewRemoteRecordingReceived unseenCount device

  sendResponseStatus status200 $ object [ "uploadURL" .= decodeUtf8 location ]
