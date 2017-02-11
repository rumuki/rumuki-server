module Handler.Subscribers where

import           Import
import           Text.Email.Validate (canonicalizeEmail)

postSubscribersR :: Handler Value
postSubscribersR = do
  now <- liftIO getCurrentTime
  subscriber' <- requireJsonEntity [] Nothing :: Handler Subscriber
  email' <- fromMaybeM (invalidArgsI [MsgInvalidEmailAddress]) $
    return . canonicalizeEmail . encodeUtf8 $ subscriberEmail subscriber'
  let subscriber = subscriber' { subscriberEmail = decodeUtf8 email'
                               , subscriberCreated = Just now }
  _ <- runDB $ insertUnique subscriber
  sendResponseStatus status201 $ object [ "subscriber" .= subscriber ]
