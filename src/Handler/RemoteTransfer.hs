module Handler.RemoteTransfer where

import           Import
import           Model.RemoteTransfer (RemoteTransferView (..),
                                       remoteTransferObjectURL,
                                       remoteTransferObjectURLFromRecordingUID,
                                       remoteTransferPublicURL)

getRemoteTransferR :: Text -> Handler Value
getRemoteTransferR ruid = do
  app <- getYesod
  let settings = appSettings app
  Entity transferID transfer <- fromMaybeM notFound $ runDB $ getBy (UniqueRemoteTransfer ruid)
  let objectURL = remoteTransferObjectURL transfer settings
  let publicURL = remoteTransferPublicURL transfer settings
  now <- liftIO getCurrentTime

  request <- parseRequest ("GET " ++ objectURL) >>= liftIO . appGoogleCloudAuthorizer app
  response <- liftIO . appHttpClient app $ request
  when (responseStatus response /= status200) notFound
  _ <- runDB $ update transferID [RemoteTransferSeen =. Just now]

  sendResponseStatus status200 $ object [
      "remoteTransfer" .= RemoteTransferView transfer publicURL ]

deleteRemoteTransferR :: Text -> Handler Value
deleteRemoteTransferR ruid = do
  app <- getYesod
  let settings = appSettings app
  _ <- runDB $ deleteBy (UniqueRemoteTransfer ruid)
  let objectURL = remoteTransferObjectURLFromRecordingUID ruid settings

  request <- parseRequest ("DELETE "
                           ++ objectURL
                           ++ appGCSBucketName settings ++ "/o/" ++ unpack ruid)
             >>= liftIO . appGoogleCloudAuthorizer app

  _ <- liftIO . appHttpClient app $ request
  sendResponseStatus status204 ()
