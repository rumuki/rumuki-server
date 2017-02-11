module Handler.PlaybackGrantSpec where

import qualified Data.ByteString.Base64 as B64
import           Data.HashMap.Strict    ((!))
import qualified Data.HashMap.Strict    as H
import           Data.Time.Clock
import qualified Database.Persist       as DB (get)
import           TestImport

spec :: Spec
spec = withApp $ do

  describe "getPlaybackGrantR" $ do

    it "returns the decryptable key cipher text" $ do
      device <- makeDevice
      (Entity gid originalGrant) <- makeGrant device id
      makeRequestWithGrantId gid
      responseSatisfies "has key cipher text" $ \(Object o) ->
        let (Object grant) = o ! "playbackGrant"
            (String ct) = grant ! "keyCipher"
            pubKey = (B64.decodeLenient . encodeUtf8) ct
        in H.member "keyCipher" grant &&
           H.member "keyOffset" grant &&
           pubKey == playbackGrantKeyCipher originalGrant

    it "removes the grant thereafter" $ do
      (_, Entity gid _) <- makeRequest
      statusIs 200
      mGrant <- runDB $ DB.get gid
      boolIsTrue "grant has been removed" $ isNothing mGrant

    it "second request fails" $ do
      (_, (Entity gid _)) <- makeRequest
      statusIs 200
      makeRequestWithGrantId gid
      statusIs 404

    it "does not return any expired grants" $ do
      aMinuteAgo <- liftIO $ addUTCTime (-60) <$> getCurrentTime
      device <- makeDevice
      (Entity gid _) <- makeGrant device $ \g -> g { playbackGrantExpires = aMinuteAgo }
      makeRequestWithGrantId gid
      statusIs 404

    where
      recordingUID = "recording123"

      makeDevice = runDB $ retrieve $ factoryDevice id

      makeGrant device transform = do
        runDB $ factoryPlaybackGrant device $ \g ->
          transform g { playbackGrantRecordingUID = recordingUID }

      makeRequestWithGrantId gid = requestJSON $ do
        setUrl (PlaybackGrantR recordingUID gid)
        setMethod "GET"

      makeRequest = do
        device <- makeDevice
        grant@(Entity gid _) <- makeGrant device id
        makeRequestWithGrantId gid
        return (device, grant)
