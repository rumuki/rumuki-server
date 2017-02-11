module Model.PlaybackGrant
       (consumeGrant)
       where

import qualified Data.HashMap.Strict as H
import           Import

instance FromJSON (RequestView PlaybackGrant) where
  parseJSON v = (return . RequestView) =<< parseJSON v

instance ToJSON (ResponseView (Entity PlaybackGrant)) where
  toJSON (ResponseView (Entity pgid pg)) = Object
                                           $ H.insert "id" (toJSON pgid)
                                           $ H.delete "keyCipher"
                                           $ H.delete "keyOffset"
                                           o
    where Object o = toJSON pg

newtype ConsumedGrant = ConsumedGrant {
  getPlaybackGrant :: Entity PlaybackGrant }

-- | The consumed grant grants access to the actual key.
-- When this type is used, we should assume that the playback
-- grant is removed straight after.
instance ToJSON ConsumedGrant where
  toJSON cg = Object
              $ H.insert "id" (toJSON gid)
              o
    where Entity gid g = getPlaybackGrant cg
          Object o     = toJSON g

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
