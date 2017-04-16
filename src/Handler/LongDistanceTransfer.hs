module Handler.LongDistanceTransfer where

import           Import
import           Model.LongDistanceTransfer ()

getLongDistanceTransferR :: Text -> Handler Value
getLongDistanceTransferR ruid = do
  app <- getYesod
  let settings = appSettings app
  transfer <- fromMaybeM notFound $ runDB $ getBy (UniqueLongDistanceTransfer ruid)

  let objectURL = "https://www.googleapis.com/storage/v1/b/"
                  ++ appGCSBucketName settings
                  ++ "/o/"
                  ++ unpack ruid

  request' <- parseRequest $ "GET " ++ objectURL
  let request = setQueryString [("key", Just . appGCSAPIKey $ settings)] request'
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

  request' <- parseRequest
              $ "DELETE https://www.googleapis.com/storage/v1/b/"
              ++ appGCSBucketName settings ++ "/o/" ++ unpack ruid
  let request = setQueryString [("key", Just . appGCSAPIKey $ settings)] request'
  _ <- liftIO . appHttpClient app $ request

  sendResponseStatus status204 ()
