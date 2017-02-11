module Model.PushNotification where

import Import
import Data.ByteString.Builder
import Network.HTTP.Simple
import qualified Data.Text as T
import Control.Concurrent (forkIO)

data PushNotificationPlatform = APNS | GCM

data PushNotification = PushNotification { notificationToken :: Text
                                         , notificationPlatform :: PushNotificationPlatform
                                         , notificationMessage :: Text }

instance ToJSON PushNotification where
  toJSON (PushNotification { notificationToken = t
                           , notificationPlatform = p
                           , notificationMessage = m }) =
    object $ [ "token"    .= [t]
             , "platform" .= p
             , "message"  .= m ]

instance ToJSON PushNotificationPlatform where
  toJSON APNS = Number 1
  toJSON GCM  = Number 2

-- | Make push notifications for a device based on their apn token.
makeNotificationsFromDevice :: Device
                            -> Text -- ^ The notification message
                            -> [ PushNotification ] -- ^ A list of possible notifications

makeNotificationsFromDevice u m = maybe [] (:[]) $ do
  token' <- deviceApnToken u
  let token = decodeUtf8 . toStrict . toLazyByteString . byteStringHex $ token'
  return PushNotification { notificationToken = token
                          , notificationPlatform = APNS
                          , notificationMessage = m }

-- | This will send a message to the given device as a push notification
-- the response is a boolean indicating whether or not the attempt was
-- successful.
sendPushNotification :: AppSettings
                     -> Device -- ^ Device to attempt sending the notification to
                     -> Text -- ^ The message to send
                     -> IO Bool

sendPushNotification settings u m = do
  let host = appNotificationsHost settings
      port = appNotificationsPort settings
      request = setRequestHost (encodeUtf8 . T.pack $ host)
              $ setRequestPort port
              $ setRequestPath "/push"
              $ setRequestMethod "POST"
              $ setRequestBodyJSON (object [ "notifications" .= makeNotificationsFromDevice u m ])
              $ defaultRequest

  response <- httpLBS request
  return $ getResponseStatusCode response == 200

-- | Given a handler context, this will fire-and-forget a
-- push notification inside a new thread.
forkAndSendPushNotification :: Text -> Device -> Handler ()

forkAndSendPushNotification msg recipient = do
  runInnerHandler <- handlerToIO
  void $ liftIO $ forkIO $ runInnerHandler $ do
    settings <- appSettings <$> getYesod
    let isTesting = appIsTesting settings
        h :: HttpException -> Handler ()
        h = case isTesting of
          True -> const (return ())
          False -> $(logWarn) . fromString . show
    handle h $ liftIO $ sendPushNotification settings recipient msg >> return ()

forkAndSendPushNotificationI :: AppMessage -> Device -> Handler ()

forkAndSendPushNotificationI msgi recipient = do
  msg <- getMessageRender >>= (\mr -> return $ mr msgi)
  forkAndSendPushNotification msg recipient
