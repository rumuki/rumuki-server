module Model.ConsumedGrant ( consumePlaybackGrant
                           , consumePerpetualGrant
                           ) where

import           Data.Aeson
import qualified Data.HashMap.Strict  as H
import           Import
import           Model.PerpetualGrant ()
import           Model.PlaybackGrant  ()

data ConsumedGrant = ConsumedPlaybackGrant (Entity PlaybackGrant)
                   | ConsumedPerpetualGrant (Entity PerpetualGrant)

-- | The consumed grant grants access to the actual key.
-- When this type is used, we should assume that the playback
-- grant is removed straight after.
instance ToJSON ConsumedGrant where

  toJSON (ConsumedPlaybackGrant grant@(Entity _ g)) =
   Object $
    H.insert "keyCipher" (toJSON $ playbackGrantKeyCipher g) $
    H.insert "keyOffset" (toJSON $ playbackGrantKeyOffset g) o
    where
    Object o = toJSON grant

  toJSON (ConsumedPerpetualGrant grant@(Entity _ g)) =
   Object $
    H.insert "keyCipher" (toJSON $ perpetualGrantKeyCipher g) o
    where
    Object o = toJSON grant


-- | Ensure that the following two functions are the only way to retrieve
-- a consumed grant:

consumePlaybackGrant :: Entity PlaybackGrant -> Handler ConsumedGrant
consumePlaybackGrant grant@(Entity gid _) = do
  runDB $ delete gid
  return $ ConsumedPlaybackGrant grant

consumePerpetualGrant :: Entity PerpetualGrant -> Handler ConsumedGrant
consumePerpetualGrant grant = return $ ConsumedPerpetualGrant grant
