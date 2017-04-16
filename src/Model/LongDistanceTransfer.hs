module Model.LongDistanceTransfer () where

import           Data.Aeson
import           Import

instance ToJSON (Entity LongDistanceTransfer) where
  toJSON (Entity _ t) = object [
      "recordingUID"               .= longDistanceTransferRecordingUID t
    , "recordingNameCipher"        .= longDistanceTransferRecordingNameCipher t
    , "senderKeyFingerprintCipher" .= longDistanceTransferSenderKeyFingerprintCipher t
    , "keyCipher"                  .= longDistanceTransferKeyCipher t
    , "created"                    .= longDistanceTransferCreated t
    ]
