module Handler.Subscribers where

import           Data.Time.Clock
import           Import
import           Text.Email.Validate (canonicalizeEmail)

postSubscribersR :: Handler Value
postSubscribersR = do
  now <- liftIO getCurrentTime
  subscriber' <- requireJsonEntity [] Nothing :: Handler Subscriber
  email' <- fromMaybeM (invalidArgsI [MsgInvalidEmailAddress]) $
    return . canonicalizeEmail . encodeUtf8 $ subscriberEmail subscriber'

  let roundedNow = UTCTime { utctDay = utctDay now
                           , utctDayTime = secondsToDiffTime . (*) (60 * 60) . round $
                                           utctDayTime now / (60 * 60) }

  let subscriber = subscriber' { subscriberEmail = decodeUtf8 email'
                               , subscriberCreated = Just roundedNow }
  _ <- runDB $ insertUnique subscriber
  sendResponseStatus status201 $ object [ "subscriber" .= subscriber ]
