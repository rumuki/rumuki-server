module Model.PlaybackGrant where

import           Data.Aeson
import           Import

instance ToJSON (Entity PlaybackGrant) where
  toJSON (Entity pgid pg) = object [ "id" .= pgid
                                   , "recordingUID" .= playbackGrantRecordingUID pg
                                   , "recipientKeyFingerprint" .= playbackGrantRecipientKeyFingerprint pg
                                   , "expires" .= playbackGrantExpires pg
                                   , "created" .= playbackGrantCreated pg
                                   ]
