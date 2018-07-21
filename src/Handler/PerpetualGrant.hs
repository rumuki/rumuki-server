module Handler.PerpetualGrant where

import           Import
import           Model.ConsumedGrant (consumePerpetualGrant)

getPerpetualGrantR :: Text -> PerpetualGrantId -> Handler Value
getPerpetualGrantR ruid pgid = do
  now <- liftIO getCurrentTime
  mr <- getMessageRender
  grant' <- fromMaybeM
            (sendResponseStatus status404 $ object ["message" .= mr MsgPerpetualGrantNotFound]) $
            runDB $ selectFirst
            [ PerpetualGrantId           ==. pgid
            , PerpetualGrantRecordingUID ==. ruid
            , PerpetualGrantExpires      >. now ]
            [ Asc PerpetualGrantExpires ]

  grant <- consumePerpetualGrant grant'
  sendResponseStatus status200 $ object ["perpetualGrant" .= grant]
