module Handler.ScreenCaptureDetectionsSpec
  (spec) where

import           TestImport

spec :: Spec
spec = withApp $ do

  describe "postScreenCaptureDetectionsR" $ do

    it "creates a screen capture detection" $ do
      makePOSTRequest
      statusIs 201
      boolIsTrue "detection is created" . isJust <$>
        runDB (selectFirst [ScreenCaptureDetectionRecordingUID ==. "recordinguid123"] [])

    it "succeeds even if the detection already exists" $ do
      makePOSTRequest >> statusIs 201
      makePOSTRequest >> statusIs 201

makePOSTRequest :: YesodExample App ()
makePOSTRequest = requestJSON $ do
  setUrl $ ScreenCaptureDetectionsR
  setMethod "POST"
  setRequestBody $ encode $ object
    [ "recordingUID" .= ("recordinguid123" :: Text)
    , "recipientKeyFingerprint" .= ("a1b2c3d4e5f6" :: ByteString) ]





