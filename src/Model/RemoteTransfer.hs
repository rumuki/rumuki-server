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
    , "type"                  .= remoteTransferType t
    , "created"               .= remoteTransferCreated t ]

data RemoteTransferView = RemoteTransferView
                          RemoteTransfer
                          String

instance ToJSON RemoteTransferView where
  toJSON (RemoteTransferView remoteTransfer downloadURL) =
    Object $ H.insert "downloadURL" (String $ pack downloadURL) o
    where Object o = toJSON remoteTransfer

remoteTransferObjectURL :: RemoteTransfer -> AppSettings -> String
remoteTransferObjectURL t = remoteTransferObjectURLFromRecordingUID
                            $ remoteTransferRecordingUID t

remoteTransferPublicURL :: RemoteTransfer -> AppSettings -> String
remoteTransferPublicURL t = remoteTransferPublicURLFromRecordingUID
  $ remoteTransferRecordingUID t

remoteTransferObjectURLFromRecordingUID :: Text -> AppSettings -> String
remoteTransferObjectURLFromRecordingUID ruid settings =
  "https://www.googleapis.com/storage/v1/b/"
  ++ appGCSBucketName settings
  ++ "/o/"
  ++ unpack ruid

remoteTransferPublicURLFromRecordingUID :: Text -> AppSettings -> String
remoteTransferPublicURLFromRecordingUID ruid settings =
  "https://"
  ++ appGCSBucketName settings
  ++ ".storage.googleapis.com/"
  ++ unpack ruid
