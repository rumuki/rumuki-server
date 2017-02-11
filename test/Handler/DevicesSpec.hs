module Handler.DevicesSpec
       (spec) where

import           TestImport

spec :: Spec
spec = withApp $ testDelete >> testPost

testDelete :: SpecWith (TestApp App)
testDelete =

  describe "deleteDevicesR" $ do

    it "removes the device object" $ do
      _ <- runDB $ retrieve $ factoryDevice $ \d -> d { deviceToken = "thisisadevicetoken" }
      makeRequest []
      d' <- runDB $ selectFirst [DeviceToken ==. "thisisadevicetoken"] []
      assertEq "Device no longer exists" True $ isNothing d'

    it "removes playback grants for the given recordings" $ do
      d <- runDB $ retrieve $ factoryDevice $ \d -> d { deviceToken = "thisisadevicetoken" }
      _ <- runDB $ factoryPlaybackGrant d $ \pg -> pg { playbackGrantRecordingUID = "recording123" }
      _ <- runDB $ factoryPlaybackGrant d $ \pg -> pg { playbackGrantRecordingUID = "recording1234" }
      _ <- runDB $ factoryPlaybackGrant d $ \pg -> pg { playbackGrantRecordingUID = "recording12345" }
      let uids = ["recording123", "recording1234", "recording12345"]
      makeRequest uids
      sequence $ (flip map) uids $ \ruid -> do
        p' <- runDB $ selectFirst [PlaybackGrantRecordingUID ==. ruid] []
        assertEq "Grant no longer exists" True $ isNothing p'

  where makeRequest ruids = requestJSON $ do
          setMethod "DELETE"
          setUrl DevicesR
          setRequestBody $ encode $
            object [ "deviceToken" .= ("thisisadevicetoken" :: Text)
                   , "recordingUIDs" .= (ruids :: [Text]) ]

testPost :: SpecWith (TestApp App)
testPost =
  describe "postDevicesR" $ do

    it "creates a device" $ do
      makeRequest id id
      statusIs 201
      s <- runDB $ selectFirst [DeviceKeyFingerprint ==. "keyfingerprint"] []
      assertEq "Got device" True $ isJust s

    it "allows updating the apn token" $ do
      makeRequest id id
      statusIs 201
      makeRequest id $ const (Just "newapntoken" :: Maybe ByteString)
      statusIs 201
      s' <- runDB $ selectFirst [DeviceApnToken ==. (Just "newapntoken" :: Maybe ByteString)] []
      assertEq "Device has updated APN token" True $ isJust s'

    it "allows updating the fingerprint" $ do
      makeRequest id id
      statusIs 201
      makeRequest (const ("newfingerprint" :: ByteString)) id
      statusIs 201
      s' <- runDB $ selectFirst [DeviceKeyFingerprint ==. ("newfingerprint" :: ByteString)] []
      assertEq "Device has updated fingerprint" True $ isJust s'

  where
    makeRequest transformFingerprint transformApnToken = requestJSON $ do
      setMethod "POST"
      setUrl DevicesR
      setRequestBody $ encode $
        object [ "deviceToken" .= ("thisisadevicetoken" :: Text)
               , "keyFingerprint" .= transformFingerprint ("keyfingerprint" :: ByteString)
               , "apnDeviceToken" .= transformApnToken (Nothing :: Maybe ByteString) ]
