module Handler.RemoteTransfer where

import           Import
import           Model.RemoteTransfer (remoteTransferObjectURL,
                                       remoteTransferObjectURLFromRecordingUID,
                                       remoteTransferPublicURL)

getRemoteTransferR :: Text -> Handler Value
getRemoteTransferR ruid = do
  app <- getYesod
  Entity _ transfer <- fromMaybeM notFound $ runDB $ getBy (UniqueRemoteTransfer ruid)
  objectURL <- remoteTransferObjectURL transfer
  publicURL <- remoteTransferPublicURL transfer

  request <- parseRequest ("GET " ++ objectURL) >>= liftIO . appGoogleCloudAuthorizer app
  response <- liftIO . appHttpClient app $ request
  when (responseStatus response /= status200) $ notFound

  sendResponseStatus status200 $ object [
      "remoteTransfer" .= transfer
    , "downloadURL" .= publicURL ]

deleteRemoteTransferR :: Text -> Handler Value
deleteRemoteTransferR ruid = do
  app <- getYesod
  let settings = appSettings app
  _ <- runDB $ deleteBy (UniqueRemoteTransfer ruid)
  objectURL <- remoteTransferObjectURLFromRecordingUID ruid

  request <- parseRequest ("DELETE "
                           ++ objectURL
                           ++ appGCSBucketName settings ++ "/o/" ++ unpack ruid)
             >>= liftIO . appGoogleCloudAuthorizer app

  _ <- liftIO . appHttpClient app $ request
  sendResponseStatus status204 ()
