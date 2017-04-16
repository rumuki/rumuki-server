module Handler.LongDistanceTransfersSpec
       (spec) where

import qualified Data.ByteString.Lazy         as BSL
import           Data.HashMap.Strict          ((!))
import           Data.Time.Clock
import           Network.HTTP.Client.Internal (Request (..), Response (..),
                                               ResponseClose (..),
                                               createCookieJar)
import           Network.HTTP.Types           (http11, status200)
import           TestImport

spec :: Spec
spec = withAppAndMockResponder mockResponder $ testPost

responseLocation :: ByteString
responseLocation = "https://www.googleapis.com/upload/storage/v1/b/rumuki/o"
                   ++ "?uploadType=resumable&upload_id=xa298sd_sdlkj2"

testPost :: SpecWith (TestApp App)
testPost = describe "postLongDistanceTransfers" $ do

  it "returns the upload url" $ do
    makeRequest
    statusIs 200
    responseSatisfies "url is returned"  $ \(Object o) ->
      let (String u) = o ! "uploadURL"
      in u == decodeUtf8 responseLocation

  it "handles recording UID conflicts" $ do
    now <- lift $ getCurrentTime
    _ <- runDB $ insert $ LongDistanceTransfer recordingUID "" "" "" "" now
    makeRequest
    statusIs 400

  where
    recordingUID = "recordinguid123"
    makeRequest = requestJSON $ do
      setUrl LongDistanceTransfersR
      setMethod "POST"
      setRequestBody $ encode $
        object [ "recordingUID"               .= (recordingUID :: Text)
               , "recipientKeyFingerprint"    .= ("recipientkeyfingerprint" :: ByteString)
               , "recordingNameCipher"        .= ("recordingnameciphertext" :: ByteString)
               , "senderKeyFingerprintCipher" .= ("senderkeyfingerprintcipher" :: ByteString)
               , "keyCipher"                  .= ("keycipher" :: ByteString)
               ]

mockResponder :: Request -> Response BSL.ByteString

mockResponder
  Request { host = "www.googleapis.com"
          , path = "/upload/storage/v1/b/rumuki/o"
          , method = methodPost
          , redirectCount = 0 } =
  Response { responseStatus = status200
           , responseVersion = http11
           , responseHeaders = [ ("Location", responseLocation)
                               , ("Content-Length", "0") ]
           , responseBody = ""
           , responseCookieJar = createCookieJar []
           , responseClose' = ResponseClose $ return ()
           }

mockResponder r = error $ "Unexpected request: " ++ show r
