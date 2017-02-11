module Handler.PlaybackGrant where

import           Import
import           Model.PlaybackGrant (consumeGrant)

getPlaybackGrantR :: Text -> PlaybackGrantId -> Handler Value
getPlaybackGrantR _ pgid = do
  now <- liftIO getCurrentTime
  mr <- getMessageRender
  grant' <- fromMaybeM
            (sendResponseStatus status404 $ object ["message" .= mr MsgPlaybackGrantNotFound]) $
            runDB $ selectFirst
            [ PlaybackGrantId           ==. pgid
            , PlaybackGrantExpires      >. now ]
            [ Asc PlaybackGrantExpires ]

  grant <- runDB $ consumeGrant grant'
  sendResponseStatus status200 $ object [
                "playbackGrant" .= grant ]
