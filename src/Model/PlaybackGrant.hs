module Model.PlaybackGrant
       (consumeGrant)
       where

import Data.Aeson
import qualified Data.HashMap.Strict as H
import           Import

instance ToJSON (Entity PlaybackGrant) where
  toJSON (Entity pgid pg) = object [ "id" .= pgid
                                   , "recordingUID" .= playbackGrantRecordingUID pg
                                   , "recipientKeyFingerprint" .= playbackGrantRecipientKeyFingerprint pg
                                   , "expires" .= playbackGrantExpires pg
                                   , "created" .= playbackGrantCreated pg
                                   ]

newtype ConsumedGrant = ConsumedGrant {
  getPlaybackGrant :: Entity PlaybackGrant }

-- | The consumed grant grants access to the actual key.
-- When this type is used, we should assume that the playback
-- grant is removed straight after.
instance ToJSON ConsumedGrant where
  toJSON cg = Object $
              H.insert "keyCipher" (toJSON $ playbackGrantKeyCipher g) $
              H.insert "keyOffset" (toJSON $ playbackGrantKeyOffset g) $
              o
    where grant@(Entity _ g) = getPlaybackGrant cg
          Object o = toJSON grant

-- | The only way to create a consumed playback grant representation.
-- ensures that whenever a representation is created, the playback
-- grant gets removed.
consumeGrant ::  ( YesodPersist site
                 , YesodPersistBackend site ~ SqlBackend
                 , PersistStore (YesodPersistBackend site) ) =>
                 (Entity PlaybackGrant) ->
                 YesodDB site (ConsumedGrant)

consumeGrant grant@(Entity gid _) = do
  delete gid
  return $ ConsumedGrant grant
