module Model.RemoteTransfer ( remoteTransferObjectURL
                            , remoteTransferObjectURLFromRecordingUID
                            , remoteTransferPublicURL
                            , remoteTransferPublicURLFromRecordingUID ) where

import           Data.Aeson
import           Import

instance ToJSON (Entity RemoteTransfer) where
  toJSON (Entity _ t) = toJSON t

instance ToJSON RemoteTransfer where
  toJSON t = object [
      "recordingUID"               .= remoteTransferRecordingUID t
    , "recordingNameCipher"        .= remoteTransferRecordingNameCipher t
    , "senderKeyFingerprintCipher" .= remoteTransferSenderKeyFingerprintCipher t
    , "keyCipher"                  .= remoteTransferKeyCipher t
    , "created"                    .= remoteTransferCreated t ]

remoteTransferObjectURL :: RemoteTransfer -> Handler String
remoteTransferObjectURL t =
  remoteTransferObjectURLFromRecordingUID
  $ remoteTransferRecordingUID t

remoteTransferPublicURL :: RemoteTransfer -> Handler String
remoteTransferPublicURL t =
  remoteTransferPublicURLFromRecordingUID
  $ remoteTransferRecordingUID t

remoteTransferObjectURLFromRecordingUID :: Text -> Handler String
remoteTransferObjectURLFromRecordingUID ruid = do
  app <- getYesod
  let settings = appSettings app
  return
    $ "https://www.googleapis.com/storage/v1/b/"
    ++ appGCSBucketName settings
    ++ "/o/"
    ++ unpack ruid

remoteTransferPublicURLFromRecordingUID :: Text -> Handler String
remoteTransferPublicURLFromRecordingUID ruid = do
  app <- getYesod
  let settings = appSettings app
  return
    $ "https://"
    ++ appGCSBucketName settings
    ++ ".storage.googleapis.com/"
    ++ unpack ruid
