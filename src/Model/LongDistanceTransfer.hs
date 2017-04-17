module Model.LongDistanceTransfer ( longDistanceTransferObjectURL
                                  , longDistanceTransferObjectURLFromRecordingUID ) where

import           Data.Aeson
import           Import

instance ToJSON (Entity LongDistanceTransfer) where
  toJSON (Entity _ t) = toJSON t

instance ToJSON LongDistanceTransfer where
  toJSON t = object [
      "recordingUID"               .= longDistanceTransferRecordingUID t
    , "recordingNameCipher"        .= longDistanceTransferRecordingNameCipher t
    , "senderKeyFingerprintCipher" .= longDistanceTransferSenderKeyFingerprintCipher t
    , "keyCipher"                  .= longDistanceTransferKeyCipher t
    , "created"                    .= longDistanceTransferCreated t ]

longDistanceTransferObjectURL :: LongDistanceTransfer -> Handler String
longDistanceTransferObjectURL t =
  longDistanceTransferObjectURLFromRecordingUID
  $ longDistanceTransferRecordingUID t

longDistanceTransferObjectURLFromRecordingUID :: Text -> Handler String
longDistanceTransferObjectURLFromRecordingUID ruid = do
  app <- getYesod
  let settings = appSettings app
  return
    $ "https://www.googleapis.com/storage/v1/b/"
    ++ appGCSBucketName settings
    ++ "/o/"
    ++ unpack ruid
