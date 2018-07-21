module Handler.PlaybackGrantsSpec
       (spec) where

import qualified Data.ByteString.Lazy as BSL
import           Data.HashMap.Strict  ((!))
import qualified Data.HashMap.Strict  as H
import           Data.Time.Clock
import qualified Data.Vector          as V
import           TestImport

spec :: Spec
spec = withApp $ testGet >> testPost >> testDelete

testGet :: SpecWith (TestApp App)
testGet = describe "getPlaybackGrantsR" $ do

    it "returns a list of grants" $ do
      recipient <- makeRecipient
      _ <- makeGrant recipient
      _ <- makeGrant recipient
      _ <- makeGrant recipient
      makeRequest
      statusIs 200
      responseSatisfies "has a list of grants" $ \(Object v) ->
        let (Array grants) = v ! "playbackGrants"
        in length grants == 3

    it "includes grants that have been made by the current device" $ do
      recipient <- makeRecipient
      other <- makeOtherRecipient
      _ <- makeGrant recipient
      _ <- makeGrant other
      makeRequest
      statusIs 200
      responseSatisfies "has 2 grants" $ \(Object v) ->
        let (Array grants) = v ! "playbackGrants"
        in length grants == 2

    it "does not return the key cipher or offset" $ do
      recipient <- makeRecipient
      _ <- makeGrant recipient
      makeRequest
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
      recipient <- makeRecipient
      _ <- makeGrant recipient
      _ <- runDB $ factoryPlaybackGrant recipient $ \g ->
        g { playbackGrantRecordingUID = recordingUID
          , playbackGrantExpires = expiredTime }
      makeRequest
      statusIs 200
      responseSatisfies "has a list of grants" $ \(Object v) ->
        let (Array grants) = v ! "playbackGrants"
        in length grants == 1

  where
    recordingUID = "recordinguid123"
    makeRecipient = runDB $ retrieve $ factoryDevice id
    makeOtherRecipient = runDB $ retrieve $ factoryDevice $ \d -> d { deviceToken = "otherdevice123123-123123123-1231231" }
    makeGrant device = runDB $ factoryPlaybackGrant device $ \g -> g { playbackGrantRecordingUID = recordingUID }
    makeRequest =
      requestJSON
      $ setUrl (PlaybackGrantsR recordingUID)
      >> setMethod "GET"


testPost :: SpecWith (TestApp App)
testPost = describe "postPlaybackGrantsR" $ do

    it "takes a base64 key encrypted with the recipients pubkey" $ do
      device <- setupDevice
      makeRequest device mockKey id
      statusIs 201

    it "creates a new playback grant entry" $ do
      makeAndVerifyRequest id
      pg <- runDB $ selectFirst [] [] :: YesodExample App (Maybe (Entity PlaybackGrant))
      boolIsTrue "has new playback grant" $ isJust pg

    it "stores the key as a bytestring" $ do
      device <- setupDevice
      makeRequest device mockKey id
      statusIs 201
      (Entity _ pg) <- runDB $ retrieve $ selectFirst [] []
      boolIsTrue "stored key is identical" $ mockKey == playbackGrantKeyCipher pg

    it "stores the key offset" $ do
      makeAndVerifyRequest $ H.insert "keyOffset" (toJSON (1 :: Int))
      (Entity _ pg) <- runDB $ retrieve $ selectFirst [] []
      boolIsTrue "correct key offset" $ (playbackGrantKeyOffset pg) == Just 1

    it "sets an expiry date under 7 days" $ do
      makeAndVerifyRequest id
      now <- liftIO getCurrentTime
      let add24h = addUTCTime (60 * 60 * 24 * 7)
      (Entity _ pg) <- runDB $ retrieve $ selectFirst [] []
      boolIsTrue "expiry under 7 days" $ playbackGrantExpires pg <= add24h now

    it "sets the correct recipient" $ do
      device@(Entity _ d) <- setupDevice
      makeRequest device mockKey id
      statusIs 201
      (Entity _ pg) <- runDB $ retrieve $ selectFirst [] []
      boolIsTrue "correct recipient fingerprint" $
        playbackGrantRecipientKeyFingerprint pg ==
        deviceKeyFingerprint d

    it "succeeds even if the recipient fingerprint isn't registered" $ do
      device <- setupDevice
      makeRequest device mockKey $
        H.insert "recipientKeyFingerprint" . toJSON $
        ("lkjlkjasdaklsdjlkj" :: ByteString)
      statusIs 201

    it "responds error when the recipient fingerprint field is empty" $ do
      device <- setupDevice
      makeRequest device mockKey $ H.delete "recipientKeyFingerprint"
      statusIs 400

    it "respond with a json representation of the created grant" $ do
      makeAndVerifyRequest id
      responseSatisfies "response has correct keys" $ \(Object v) ->
        let (Object o) = v ! "playbackGrant"
        in not (H.member "keyCipher" o) &&
           H.member "recipientKeyFingerprint" o &&
           H.member "id" o &&
           H.member "recordingUID" o &&
           H.member "expires" o

  where
    mockKey = BSL.toStrict $ take 256 $ BSL.repeat 0x11
    recordingUID = "abc123kjmca"

    setupDevice = runDB $ retrieve $ factoryDevice id

    makeRequest (Entity _ d) key transform = do
      let (Object defaults) = object [ "keyCipher" .= key
                                     , "recipientKeyFingerprint" .= deviceKeyFingerprint d ]
      requestJSON $ do
        setUrl $ PlaybackGrantsR recordingUID
        setMethod "POST"
        setRequestBody $ encode $ Object $ transform defaults

    makeAndVerifyRequest transform = do
      device <- setupDevice
      makeRequest device mockKey transform
      statusIs 201


testDelete :: SpecWith (TestApp App)
testDelete = describe "deletePlaybackGrantsR" $ do

  it "removes all grants" $ do
    recipient <- makeRecipient
    other <- makeOther
    _ <- makeGrant recipient
    _ <- makeGrant recipient
    _ <- makeGrant other
    requestJSON
      $ setUrl (PlaybackGrantsR recordingUID)
      >> setMethod "DELETE"
    statusIs 204
    pgs <- runDB $ selectList [] [] :: YesodExample App [(Entity PlaybackGrant)]
    boolIsTrue "zero remaining" $ length pgs == 0

  it "removes only grants sent to a recipient if a key fingerprint is given" $ do
    recipient@(Entity _ d) <- makeRecipient
    other <- makeOther
    _ <- makeGrant recipient
    _ <- makeGrant recipient
    _ <- makeGrant other
    requestJSON
      $ setUrl (PlaybackGrantsR recordingUID)
      >> setRequestBody (encode $ object [ "recipientKeyFingerprint" .= deviceKeyFingerprint d ])
      >> setMethod "DELETE"
    statusIs 204
    pgs <- runDB $ selectList [] [] :: YesodExample App [(Entity PlaybackGrant)]
    boolIsTrue "one remaining" $ length pgs == 1

  where
    recordingUID = "recordinguid123"
    makeRecipient = runDB $ retrieve $ factoryDevice id
    makeOther = runDB $ retrieve $ factoryDevice
                $ \d -> d { deviceToken = "other-device"
                          , deviceKeyFingerprint = "other-device-fingerprint"
                          }
    makeGrant device = do
      runDB $ factoryPlaybackGrant device $ \g ->
        g { playbackGrantRecordingUID = recordingUID }
