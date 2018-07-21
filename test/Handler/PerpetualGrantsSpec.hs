module Handler.PerpetualGrantsSpec
       (spec) where

import qualified Data.ByteString.Lazy as BSL
import           Data.HashMap.Strict  ((!))
import qualified Data.HashMap.Strict  as H
import           Data.List            (head)
import           Data.Time.Clock
import           TestImport           hiding (head)

spec :: Spec
spec = withApp $ testGet >> testPost >> testDelete

data GETResponse = GETResponse {
  getResponsePerpetualGrants :: [Object] }

instance FromJSON GETResponse where
  parseJSON = withObject "response" $ \o ->
    GETResponse <$> o .: "perpetualGrants"

testGet :: SpecWith (TestApp App)
testGet = describe "getPerpetualGrantsR" $ do

    it "returns an empty list if no grant exists" $ do
      makeRequest
      statusIs 200
      responseSatisfies "empty list"
        $ (== 0) . length . getResponsePerpetualGrants

    it "returns a single grant if it is available" $ do
      recipient <- makeRecipient
      _ <- makeGrant recipient
      makeRequest
      statusIs 200
      responseSatisfies "has a single grant"
        $ (== 1) . length . getResponsePerpetualGrants

    it "returns both grants if they are available" $ do
      recipient <- makeRecipient
      otherRecipient <- makeOtherRecipient
      _ <- makeGrant recipient
      _ <- makeGrant otherRecipient
      makeRequest
      statusIs 200
      responseSatisfies "has two grants"
        $ (== 2) . length . getResponsePerpetualGrants

    it "does not return the key cipher" $ do
      recipient <- makeRecipient
      _ <- makeGrant recipient
      makeRequest
      statusIs 200
      responseSatisfies "no key or cipher text"
        $ (notMember "keyCipher") . head . getResponsePerpetualGrants

    it "does not return grants that have expired" $ do
      expiredTime <- liftIO $ addUTCTime (-60) <$> getCurrentTime
      recipient <- makeRecipient
      _ <- runDB $ factoryPerpetualGrant recipient $ \g ->
        g { perpetualGrantRecordingUID = recordingUID
          , perpetualGrantExpires = expiredTime }
      makeRequest
      statusIs 200
      responseSatisfies "no grants are returned"
        $ (== 0) . length . getResponsePerpetualGrants

  where
    recordingUID = "recordinguid123"
    makeRequest = requestJSON $ setUrl (PerpetualGrantsR recordingUID) >> setMethod "GET"
    makeRecipient = runDB $ retrieve $ factoryDevice id

    makeGrant device = runDB $ factoryPerpetualGrant device
      $ \g -> g { perpetualGrantRecordingUID = recordingUID }

    makeOtherRecipient = runDB $ retrieve $ factoryDevice
      $ \d -> d { deviceToken = "otherdevice"
                , deviceKeyFingerprint = "otherfingerprint123" }

testPost :: SpecWith (TestApp App)
testPost = describe "postPerpetualGrantsR" $ do

    it "takes a base64 key encrypted with the recipients pubkey" $ do
      device <- setupDevice
      makeRequest device mockKey id
      statusIs 201

    it "creates a new perpetual grant entry" $ do
      makeAndVerifyRequest id
      pg <- runDB $ selectFirst [] [] :: YesodExample App (Maybe (Entity PerpetualGrant))
      boolIsTrue "has new perpetual grant" $ isJust pg

    it "stores the key as a bytestring" $ do
      device <- setupDevice
      makeRequest device mockKey id
      statusIs 201
      (Entity _ pg) <- runDB $ retrieve $ selectFirst [] []
      boolIsTrue "stored key is identical" $ mockKey == perpetualGrantKeyCipher pg

    it "sets an expiry date under 1 year" $ do
      makeAndVerifyRequest id
      now <- liftIO getCurrentTime
      let add1Year = addUTCTime (60 * 60 * 24 * 365)
      (Entity _ pg) <- runDB $ retrieve $ selectFirst [] []
      boolIsTrue "expiry under 1 year" $ perpetualGrantExpires pg <= add1Year now

    it "sets the correct recipient" $ do
      device@(Entity _ d) <- setupDevice
      makeRequest device mockKey id
      statusIs 201
      (Entity _ pg) <- runDB $ retrieve $ selectFirst [] []
      boolIsTrue "correct recipient fingerprint" $
        perpetualGrantRecipientKeyFingerprint pg ==
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

    it "succeeds even when a grant already exists" $ do
      device <- setupDevice
      makeRequest device mockKey id
      statusIs 201
      pg <- runDB $ selectFirst [] [] :: YesodExample App (Maybe (Entity PerpetualGrant))
      boolIsTrue "has new perpetual grant" $ isJust pg
      makeRequest device mockKey id
      statusIs 201
      pgs <- runDB $ selectList [] [] :: YesodExample App [Entity PerpetualGrant]
      boolIsTrue "only one perpetual grant" $ length pgs == 1

    it "respond with a json representation of the created grant" $ do
      makeAndVerifyRequest id
      responseSatisfies "response has correct keys" $ \(Object v) ->
        let (Object o) = v ! "perpetualGrant"
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
        setUrl $ PerpetualGrantsR recordingUID
        setMethod "POST"
        setRequestBody $ encode $ Object $ transform defaults

    makeAndVerifyRequest transform = do
      device <- setupDevice
      makeRequest device mockKey transform
      statusIs 201


testDelete :: SpecWith (TestApp App)
testDelete = describe "deletePerpetualGrantsR" $ do

  it "removes grants sent to the given recipient" $ do
    recipient@(Entity _ d) <- makeRecipient
    other <- makeOther
    _ <- makeGrant recipient
    _ <- makeGrant other
    requestJSON $ do
      setUrl (PerpetualGrantsR recordingUID)
      setMethod "DELETE"
      setRequestBody $ encode $ object [ "recipientKeyFingerprint" .= deviceKeyFingerprint d ]
    statusIs 204
    pgs <- runDB $ selectList [] [] :: YesodExample App [(Entity PerpetualGrant)]
    boolIsTrue "one  remaining" $ length pgs == 1

  where
    recordingUID = "recordinguid123"
    makeRecipient = runDB $ retrieve $ factoryDevice id
    makeOther = runDB $ retrieve $ factoryDevice
                $ \d -> d { deviceToken = "other-device"
                          , deviceKeyFingerprint = "other-device-fingerprint"
                          }
    makeGrant device = do
      runDB $ factoryPerpetualGrant device $ \g ->
        g { perpetualGrantRecordingUID = recordingUID }
