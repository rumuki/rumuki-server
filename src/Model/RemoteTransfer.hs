module Model.RemoteTransfer ( remoteTransferObjectURL
                            , remoteTransferObjectURLFromRecordingUID
                            , remoteTransferPublicURL
                            , remoteTransferPublicURLFromRecordingUID
                            , RemoteTransferView (..)) where

import           Data.Aeson
import qualified Data.HashMap.Strict as H
import           Import

instance ToJSON (Entity RemoteTransfer) where
  toJSON (Entity _ t) = toJSON t

instance ToJSON RemoteTransfer where
  toJSON t = object [
      "recordingUID"          .= remoteTransferRecordingUID t
    , "recordingNameCipher"   .= remoteTransferRecordingNameCipher t
    , "senderPublicKeyCipher" .= remoteTransferSenderPublicKeyCipher t
    , "senderNicknameCipher"  .= remoteTransferSenderNicknameCipher t
    , "keyCipher"             .= remoteTransferKeyCipher t
    , "created"               .= remoteTransferCreated t ]

data RemoteTransferView = RemoteTransferView
                          RemoteTransfer
                          String

instance ToJSON RemoteTransferView where
  toJSON (RemoteTransferView remoteTransfer downloadURL) =
    Object $ H.insert "downloadURL" (String $ pack downloadURL) o
    where Object o = toJSON remoteTransfer

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
