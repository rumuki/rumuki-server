module Model.PerpetualGrant where

import           Data.Aeson
import           Import

instance ToJSON (Entity PerpetualGrant) where
  toJSON (Entity pgid pg) = object
    [ "id"                      .= pgid
    , "recordingUID"            .= perpetualGrantRecordingUID pg
    , "recipientKeyFingerprint" .= perpetualGrantRecipientKeyFingerprint pg
    , "expires"                 .= perpetualGrantExpires pg
    , "created"                 .= perpetualGrantCreated pg
    ]
