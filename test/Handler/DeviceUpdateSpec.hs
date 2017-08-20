module Handler.DeviceUpdateSpec (spec) where

import           Data.HashMap.Strict          ((!))
import qualified Data.HashMap.Strict          as H
import           Data.Time.Clock
import qualified Data.Vector                  as V
import           Network.HTTP.Client.Internal (Response (..))
import           Network.HTTP.Types           (status404)
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

  withAppAndMockResponder (makeMockResponder id) $ do

    describe "postDeviceUpdateR (GCS object exists)" $ do

      it "returns the remote transfers list" $ do
        device <- makeDevice
        _ <- makeRemoteTransfer device
        makeRequest device
        statusIs 200
        responseSatisfies "includes the remote transfer" $ \(Object v) ->
          let (Array transfers) = v ! "remoteTransfers"
          in length transfers == 1

      it "returns any new playback grants on the remote transfers" $ do
        device@(Entity _ d) <- makeDevice
        _ <- makeGrant device
        _ <- makeRemoteTransfer device
        makeRequest device

        requestJSON $ do
          setUrl $ DeviceUpdateR
          setMethod "POST"
          setRequestBody $
            encode $
            -- Empty recording uids so that we can test the grant still
            -- appears properly.
            object [ "recordingUIDs" .= ([] :: [Text])
                   , "deviceKeyFingerprint" .= deviceKeyFingerprint d ]

        statusIs 200
        responseSatisfies "includes the remote transfer" $ \(Object v) ->
          let (Array transfers) = v ! "remoteTransfers"
              (Array grants) = v ! "playbackGrants"
          in length transfers == 1 &&
             length grants == 1

      it "sets the seen property on the remote transfer" $ do
        device <- makeDevice
        _ <- makeRemoteTransfer device
        makeRequest device
        statusIs 200
        unseenRemoteTransfers <- runDB $ selectList  [ RemoteTransferSeen ==. Nothing ] []
        assertEq "All remote transfers are now seen" 0 (length unseenRemoteTransfers)

      it "sets the downloadURL on the remote transfer" $ do
        device <- makeDevice
        _ <- makeRemoteTransfer device
        makeRequest device
        statusIs 200
        body <- withResponse $ return . simpleBody
        let maybeDecoded = decode body :: Maybe Value
        let maybeURL = parseMaybe parseDownloadURL =<< maybeDecoded
        liftIO $ print maybeURL
        assertEq "downloadURL as expected"
          (Just "https://rumuki.storage.googleapis.com/recording123")
          maybeURL

  withAppAndMockResponder (makeMockResponder $ \r -> r { responseStatus = status404 }) $ do

    describe "postDeviceUpdateR (GCS object doesn't exist)" $ do

      it "returns an empty remote transfers list" $ do
        device <- makeDevice
        _ <- makeRemoteTransfer device
        makeRequest device
        statusIs 200
        responseSatisfies "doesn't include the remote transfer" $ \(Object v) ->
          let (Array transfers) = v ! "remoteTransfers" in length transfers == 0

  where
    recordingUID = "recording123"
    makeMockResponder = mockGCResponder "/storage/v1/b/rumuki/o/recording123" "GET"
    makeDevice = runDB $ retrieve $ factoryDevice id
    makeGrant device = runDB $ factoryPlaybackGrant device $ \g -> g { playbackGrantRecordingUID = recordingUID }
    makeRemoteTransfer (Entity _ d) = runDB $ factoryRemoteTransfer
      $ \t -> t { remoteTransferRecipientKeyFingerprint = deviceKeyFingerprint d }
    makeRequest (Entity _ d) = requestJSON $ do
      setUrl $ DeviceUpdateR
      setMethod "POST"
      setRequestBody $ encode $ object [ "recordingUIDs" .= ([recordingUID] :: [Text])
                                       , "deviceKeyFingerprint" .= deviceKeyFingerprint d ]
    parseDownloadURL :: Value -> Parser Text
    parseDownloadURL = withObject "response" $ \o -> do
      transfers <- o .: "remoteTransfers"
      let firstTransfer = V.head transfers
      url <- firstTransfer .: "downloadURL"
      withText "url" return $ url
