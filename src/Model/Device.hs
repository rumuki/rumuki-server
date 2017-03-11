module Model.Device (countUnseenPlaybackGrants) where

import           Data.Time.Clock
import           Import

countUnseenPlaybackGrants :: Device -> Handler Int
countUnseenPlaybackGrants device = do
  now <- liftIO getCurrentTime
  let inferredLastAccess = fromMaybe (addUTCTime (-1 * 60 * 60 * 24 * 30) now) $ deviceUpdated device
  runDB $ count [ PlaybackGrantExpires >. now
                , PlaybackGrantRecipientKeyFingerprint ==. deviceKeyFingerprint device
                , PlaybackGrantCreated >. inferredLastAccess ]
