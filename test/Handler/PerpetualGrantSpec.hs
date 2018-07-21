module Handler.PerpetualGrantSpec where

import qualified Data.ByteString.Base64 as B64
import           Data.HashMap.Strict    ((!))
import qualified Data.HashMap.Strict    as H
import           Data.Time.Clock
import qualified Database.Persist       as DB (get)
import           TestImport

spec :: Spec
spec = withApp $ do

  describe "getPerpetualGrantR" $ do

    it "returns the decryptable key cipher text" $ do
      device <- makeDevice
      (Entity gid originalGrant) <- makeGrant device id
      makeRequestWithGrantId gid
      responseSatisfies "has key cipher text" $ \(Object o) ->
        let (Object grant) = o ! "perpetualGrant"
            (String ct) = grant ! "keyCipher"
            pubKey = (B64.decodeLenient . encodeUtf8) ct
        in H.member "keyCipher" grant &&
           pubKey == perpetualGrantKeyCipher originalGrant

    it "does not remove the grant thereafter" $ do
      (_, Entity gid _) <- makeRequest
      statusIs 200
      mGrant <- runDB $ DB.get gid
      boolIsTrue "grant still exists" $ isJust mGrant

    it "second request succeeds" $ do
      (_, (Entity gid _)) <- makeRequest
      statusIs 200
      makeRequestWithGrantId gid
      statusIs 200

    it "does not return any expired grants" $ do
      aMinuteAgo <- liftIO $ addUTCTime (-60) <$> getCurrentTime
      device <- makeDevice
      (Entity gid _) <- makeGrant device $ \g -> g { perpetualGrantExpires = aMinuteAgo }
      makeRequestWithGrantId gid
      statusIs 404

    it "does not succeed with a mismatched recording id" $ do
      device <- makeDevice
      (Entity gid _) <- makeGrant device $
        \g -> g { perpetualGrantRecordingUID = "notrecording123" }
      makeRequestWithGrantId gid
      statusIs 404

    where
      recordingUID = "recording123"

      makeDevice = runDB $ retrieve $ factoryDevice id

      makeGrant device transform = do
        runDB $ factoryPerpetualGrant device $ \g ->
          transform g { perpetualGrantRecordingUID = recordingUID }

      makeRequestWithGrantId gid = requestJSON $ do
        setUrl (PerpetualGrantR recordingUID gid)
        setMethod "GET"

      makeRequest = do
        device <- makeDevice
        grant@(Entity gid _) <- makeGrant device id
        makeRequestWithGrantId gid
        return (device, grant)
