module Handler.DeviceUpdateSpec (spec) where

import           Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as H
import           Data.Time.Clock
import qualified Data.Vector         as V
import           TestImport

spec :: Spec
spec = withApp $ do

  describe "getDeviceUpdateR" $ do

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

  where
    recordingUID = "recordinguid123"
    makeDevice = runDB $ retrieve $ factoryDevice id
    makeGrant device = runDB $ factoryPlaybackGrant device $ \g -> g { playbackGrantRecordingUID = recordingUID }
    makeRequest (Entity _ d) = requestJSON $ do
      setUrl $ DeviceUpdateR
      setMethod "GET"
      setRequestBody $ encode $ object [ "recordingUIDs" .= ([recordingUID] :: [Text])
                                       , "deviceKeyFingerprint" .= deviceKeyFingerprint d ]
