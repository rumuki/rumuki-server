module Handler.PlaybackGrantsSearch (getPlaybackGrantsSearchR) where

import           Data.Aeson
import           Data.Time.Clock     (getCurrentTime)
import           Import
import           Model.PlaybackGrant ()

data GETRequest = GETRequest [Text]

instance FromJSON GETRequest where
  parseJSON = withObject "playback grants search request" $ \o ->
    GETRequest <$> o .: "recordingUIDs"

getPlaybackGrantsSearchR :: Handler Value
getPlaybackGrantsSearchR = do
  now <- liftIO getCurrentTime
  GETRequest uids <- requireJsonBody
  grants <- runDB $ selectList [ PlaybackGrantRecordingUID <-. uids ] []
  sendResponseStatus status200 $ object [ "playbackGrants" .= map ResponseView grants ]
