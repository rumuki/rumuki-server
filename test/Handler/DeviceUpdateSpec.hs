module Handler.DeviceUpdateSpec (spec) where

import qualified Data.ByteString.Lazy         as BSL
import           Data.HashMap.Strict          ((!))
import qualified Data.HashMap.Strict          as H
import           Data.Time.Clock
import qualified Data.Vector                  as V
import           Network.HTTP.Client.Internal (Request (..), Response (..),
                                               ResponseClose (..),
                                               createCookieJar)
import           Network.HTTP.Types           (http11, status200, status404)
import           TestImport

spec :: Spec
spec = do

  withApp $ do

    describe "postDeviceUpdateR" $ do

        it "does not return the key cipher" $ do
          device <- makeDevice
          _ <- makeGrant device
          makeRequest device
          statusIs 200
          responseSatisfies "no key or cipher text" $ \(Object o) ->
              let (Array grants) = o ! "playbackGrants"
                  (Object grant) = V.head grants
                  d = H.difference grant $ H.fromList
                      [ ("id", Bool True)
                      , ("recordingUID", Bool True)
                      , ("recipientKeyFingerprint", Bool True)
                      , ("expires", Bool True)
                      , ("created", Bool True) ]
              in null (H.toList d)

        it "does not return grants that have expired" $ do
          expiredTime <- liftIO $ addUTCTime (-60) <$> getCurrentTime
          device <- makeDevice
          _ <- makeGrant device
          _ <- runDB $ factoryPlaybackGrant device $ \g ->
              g { playbackGrantRecordingUID = recordingUID
                , playbackGrantExpires = expiredTime }
          makeRequest device
          statusIs 200
          responseSatisfies "doesn't include the expired grant" $ \(Object v) ->
              let (Array grants) = v ! "playbackGrants"
              in length grants == 1

  withAppAndMockResponder mockResponderGet200 $ do

    describe "postDeviceUpdateR (GCS object exists)" $ do

      it "returns the long distance transfers list" $ do
        device@(Entity _ d) <- makeDevice
        _ <- runDB $ factoryLongDistanceTransfer
             $ \t -> t { longDistanceTransferRecipientKeyFingerprint = deviceKeyFingerprint d }
        makeRequest device
        statusIs 200
        responseSatisfies "includes the long distance transfer" $ \(Object v) ->
          let (Array transfers) = v ! "longDistanceTransfers"
          in length transfers == 1

  withAppAndMockResponder mockResponderGet404 $ do

    describe "postDeviceUpdateR (GCS object doesn't exist)" $ do

      it "returns an empty long distance transfers list" $ do
        device@(Entity _ d) <- makeDevice
        _ <- runDB $ factoryLongDistanceTransfer
             $ \t -> t { longDistanceTransferRecipientKeyFingerprint = deviceKeyFingerprint d }
        makeRequest device
        statusIs 200
        responseSatisfies "doesn't include the long distance transfer" $ \(Object v) ->
          let (Array transfers) = v ! "longDistanceTransfers"
          in length transfers == 0

  where
    recordingUID = "recording123"
    makeDevice = runDB $ retrieve $ factoryDevice id
    makeGrant device = runDB $ factoryPlaybackGrant device $ \g -> g { playbackGrantRecordingUID = recordingUID }
    makeRequest (Entity _ d) = requestJSON $ do
      setUrl $ DeviceUpdateR
      setMethod "POST"
      setRequestBody $ encode $ object [ "recordingUIDs" .= ([recordingUID] :: [Text])
                                       , "deviceKeyFingerprint" .= deviceKeyFingerprint d ]

mockResponderGet200 :: Request -> Response BSL.ByteString
mockResponderGet200
  Request { host               = "www.googleapis.com"
          , path               = "/storage/v1/b/rumuki/o/recording123"
          , method             = "GET" } =
  Response { responseStatus    = status200
           , responseVersion   = http11
           , responseHeaders   = [("Content-Length", "3")]
           , responseCookieJar = createCookieJar []
           , responseClose'    = ResponseClose $ return ()
           , responseBody      = "{}" } -- In reality an object resource is returned
mockResponderGet200 r = error $ "Unexpected request: " ++ show r

mockResponderGet404 :: Request -> Response BSL.ByteString
mockResponderGet404
  Request { host               = "www.googleapis.com"
          , path               = "/storage/v1/b/rumuki/o/recording123"
          , method             = "GET" } =
  Response { responseStatus    = status404
           , responseVersion   = http11
           , responseHeaders   = [("Content-Length", "0")]
           , responseCookieJar = createCookieJar []
           , responseClose'    = ResponseClose $ return ()
           , responseBody      = "" }
mockResponderGet404 r = error $ "Unexpected request: " ++ show r
