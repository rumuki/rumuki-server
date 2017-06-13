module Handler.RemoteTransfersSpec
       (spec) where

import qualified Data.ByteString.Lazy         as LB
import           Data.HashMap.Strict          ((!))
import           Network.HTTP.Client.Internal (Request, Response (..))
import           TestImport

spec :: Spec
spec = withAppAndMockResponder mockResponder $ testPost

testPost :: SpecWith (TestApp App)
testPost = describe "postRemoteTransfers" $ do

  it "returns the upload url" $ do
    makeRequest
    statusIs 200
    responseSatisfies "url is returned"  $ \(Object o) ->
      let (String u) = o ! "uploadURL"
      in u == decodeUtf8 responseLocation

  it "handles recording UID conflicts" $ do
    _ <- runDB $ factoryRemoteTransfer id
    makeRequest
    statusIs 400

  where
    recordingUID = "recording123"
    makeRequest = requestJSON $ do
      setUrl RemoteTransfersR
      setMethod "POST"
      setRequestBody $ encode $
        object [ "recordingUID"               .= (recordingUID :: Text)
               , "recipientKeyFingerprint"    .= ("recipientkeyfingerprint" :: ByteString)
               , "recordingNameCipher"        .= ("recordingnameciphertext" :: ByteString)
               , "senderKeyFingerprintCipher" .= ("senderkeyfingerprintcipher" :: ByteString)
               , "keyCipher"                  .= ("keycipher" :: ByteString) ]

responseLocation :: ByteString
responseLocation = "https://www.googleapis.com/upload/storage/v1/b/rumuki/o"
                   ++ "?uploadType=resumable&upload_id=xa298sd_sdlkj2"

mockResponder :: Request -> Response LB.ByteString
mockResponder = mockGCResponder "/upload/storage/v1/b/rumuki/o" "POST" $ \r ->
  r { responseHeaders = [ ("Location", responseLocation)
                        , ("Content-Length", "0") ] }
