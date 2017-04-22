module Handler.LongDistanceTransfers where

import           Data.Aeson          (withObject)
import           Data.Time.Clock
import           Import
import           Network.HTTP.Simple (getResponseHeader)

data POSTLongDistanceTransfersRequest =
  POSTLongDistanceTransfersRequest { recordingUID :: Text
                                   , recipientKeyFingerprint :: ByteString
                                   , recordingNameCipher :: ByteString
                                   , senderKeyFingerprintCipher :: ByteString
                                   , keyCipher :: ByteString
                                   }

instance FromJSON POSTLongDistanceTransfersRequest where
  parseJSON = withObject "transfer request object" $ \o ->
    POSTLongDistanceTransfersRequest
    <$> o .: "recordingUID"
    <*> o .: "recipientKeyFingerprint"
    <*> o .: "recordingNameCipher"
    <*> o .: "senderKeyFingerprintCipher"
    <*> o .: "keyCipher"

postLongDistanceTransfersR :: Handler Value
postLongDistanceTransfersR = do
  app <- getYesod
  req <- requireJsonBody
  now <- liftIO getCurrentTime
  let settings = appSettings app

  existingTransfer <- runDB $ getBy $ UniqueLongDistanceTransfer (recordingUID req)
  case isJust existingTransfer of
    True -> invalidArgsI [MsgTransferForRecordingAlreadyExists]
    False -> return ()

  request' <- parseRequest
              $ "POST https://www.googleapis.com/upload/storage/v1/b/"
              ++ appGCSBucketName settings
              ++ "/o"

  let zeroRedirectCount r = r { redirectCount = 0 }
  request <- liftIO
             $ appGoogleCloudAuthorizer app
             $ setQueryString [ ("uploadType", Just "resumable")
                              , ("name", Just . encodeUtf8 $ recordingUID req)
                              , ("key", Just . appGCSAPIKey $ settings) ]
             $ zeroRedirectCount
             $ request'

  response <- liftIO . appHttpClient app $ request

  when (responseStatus response /= status200) $ error "could not create upload url"

  -- The upload URL is available in the @Location@ header,
  -- and is valid for up to 7 days.
  location <- maybe (error "could not create upload url") return
              $ headMay
              $ getResponseHeader "Location" response

  _ <- fromMaybeM (error "Could not create the transfer: DB error")
       $ runDB $ insertUnique
       $ LongDistanceTransfer
         (recordingUID req)
         (recipientKeyFingerprint req)
         (recordingNameCipher req)
         (senderKeyFingerprintCipher req)
         (keyCipher req)
         now

  sendResponseStatus status200 $ object [ "uploadURL" .= decodeUtf8 location ]
