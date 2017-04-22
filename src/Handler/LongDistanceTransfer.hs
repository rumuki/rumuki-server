module Handler.LongDistanceTransfer where

import           Import
import           Model.LongDistanceTransfer (longDistanceTransferObjectURL, longDistanceTransferObjectURLFromRecordingUID)

getLongDistanceTransferR :: Text -> Handler Value
getLongDistanceTransferR ruid = do
  app <- getYesod
  Entity _ transfer <- fromMaybeM notFound $ runDB $ getBy (UniqueLongDistanceTransfer ruid)
  objectURL <- longDistanceTransferObjectURL transfer

  request <- parseRequest ("GET " ++ objectURL) >>= liftIO . appGoogleCloudAuthorizer app
  response <- liftIO . appHttpClient app $ request
  when (responseStatus response /= status200) $ notFound

  sendResponseStatus status200 $ object [
      "longDistanceTransfer" .= transfer
    , "downloadURL" .= objectURL ]

deleteLongDistanceTransferR :: Text -> Handler Value
deleteLongDistanceTransferR ruid = do
  app <- getYesod
  let settings = appSettings app
  _ <- runDB $ deleteBy (UniqueLongDistanceTransfer ruid)
  objectURL <- longDistanceTransferObjectURLFromRecordingUID ruid

  request <- parseRequest ("DELETE "
                           ++ objectURL
                           ++ appGCSBucketName settings ++ "/o/" ++ unpack ruid)
             >>= liftIO . appGoogleCloudAuthorizer app
  _ <- liftIO . appHttpClient app $ request

  sendResponseStatus status204 ()
