module Model.Device () where

import           Data.Time.Clock (getCurrentTime)

-- countUnseenPlaybackGrants :: Device -> Handler Int
-- countUnseenPlaybackGrants device = do
--   now <- liftIO getCurrentTime
--   runDB $ count [ PlaybackGrantExpires >. now
--                 , PlaybackGrantRecipientKeyFingerprint ==. deviceKeyFingerprint device ]
