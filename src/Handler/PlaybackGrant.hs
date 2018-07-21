module Handler.PlaybackGrant where

import           Import
import           Model.ConsumedGrant (consumePlaybackGrant)

getPlaybackGrantR :: Text -> PlaybackGrantId -> Handler Value
getPlaybackGrantR ruid pgid = do
  now <- liftIO getCurrentTime
  mr <- getMessageRender
  grant' <- fromMaybeM
            (sendResponseStatus status404 $ object ["message" .= mr MsgPlaybackGrantNotFound]) $
            runDB $ selectFirst
            [ PlaybackGrantId           ==. pgid
            , PlaybackGrantRecordingUID ==. ruid
            , PlaybackGrantExpires      >. now ]
            [ Asc PlaybackGrantExpires ]

  grant <- consumePlaybackGrant grant'
  sendResponseStatus status200 $ object ["playbackGrant" .= grant]
