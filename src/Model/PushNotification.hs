module Model.PushNotification where

import Import
import Data.ByteString.Builder
import Network.HTTP.Simple
import qualified Data.Text as T
import Control.Concurrent (forkIO)

data PushNotificationPlatform = APNS | GCM

instance ToJSON PushNotificationPlatform where
  toJSON APNS = Number 1
  toJSON GCM  = Number 2

data PushNotification = PushNotification { notificationToken            :: Text
                                         , notificationPlatform         :: PushNotificationPlatform
                                         , notificationMessage          :: Text
                                         , notificationContentAvailable :: Bool
                                         , notificationBadge            :: Int }

instance ToJSON PushNotification where
  toJSON n = object
    [ "token"             .= [notificationToken n]
    , "platform"          .= notificationPlatform n
    , "message"           .= notificationMessage n
    , "content_available" .= notificationContentAvailable n
    , "badge"             .= notificationBadge n ]

-- | Make push notifications for a device based on their apn token.
makeNotificationsFromDevice :: Device
                            -> Text -- ^ The notification message
                            -> Int  -- ^ Number of items to show on the badge
                            -> [ PushNotification ] -- ^ A list of possible notifications

makeNotificationsFromDevice u m b = maybeToList $ do
  token' <- deviceApnToken u
  let token = decodeUtf8 . toStrict . toLazyByteString . byteStringHex $ token'
  return PushNotification { notificationToken = token
                          , notificationPlatform = APNS
                          , notificationMessage = m
                          , notificationContentAvailable = b > 0
                          , notificationBadge = b }

-- | This will send a message to the given device as a push notification
-- the response is a boolean indicating whether or not the attempt was
-- successful.
sendPushNotification :: AppSettings
                     -> Device -- ^ Device to attempt sending the notification to
                     -> Text   -- ^ The message to send
                     -> Int    -- ^ Number of items to show on the badge
                     -> IO Bool

sendPushNotification settings u m b = do
  let host = appNotificationsHost settings
      port = appNotificationsPort settings
      request = setRequestHost (encodeUtf8 . T.pack $ host)
              $ setRequestPort port
              $ setRequestPath "/push"
              $ setRequestMethod "POST"
              $ setRequestBodyJSON (object [ "notifications" .= makeNotificationsFromDevice u m b ])
              defaultRequest

  response <- httpLBS request
  return $ getResponseStatusCode response == 200

-- | Given a handler context, this will fire-and-forget a
-- push notification inside a new thread.
forkAndSendPushNotification :: Text -> Int -> Device -> Handler ()

forkAndSendPushNotification msg badgeCount recipient = do
  runInnerHandler <- handlerToIO
  void $ liftIO $ forkIO $ runInnerHandler $ do
    settings <- appSettings <$> getYesod
    let isTesting = appIsTesting settings
        h :: HttpException -> Handler ()
        h = if isTesting then const (return ())
            else $(logWarn) . fromString . show
    handle h $ liftIO $ void $ sendPushNotification settings recipient msg badgeCount

forkAndSendPushNotificationI :: AppMessage -> Int -> Device -> Handler ()

forkAndSendPushNotificationI msgi badgeCount recipient = do
  msg <- getMessageRender >>= (\mr -> return $ mr msgi)
  forkAndSendPushNotification msg badgeCount recipient
