module Handler.LongDistanceTransferSpec
       (spec) where

import qualified Data.ByteString.Lazy         as BSL
import           Network.HTTP.Client.Internal (Request (..), Response (..),
                                               ResponseClose (..),
                                               createCookieJar)
import           Network.HTTP.Types           (http11, status200, status404)
import           TestImport

spec :: Spec
spec =  testGet >> testDelete

testGet :: Spec
testGet = do

  withAppAndMockResponder mockResponderGet200 $ describe "getLongDistanceTransfer (object exists)" $ do
    it "returns the metadata along with an URL to the object" $ do
      Entity _ t <- runDB $ factoryLongDistanceTransfer id
      requestJSON $ do
        setUrl $ LongDistanceTransferR $ longDistanceTransferRecordingUID t
        setMethod "GET"
      statusIs 200

  withAppAndMockResponder mockResponderGet404 $ describe "getLongDistanceTransfer (object doesn't exist)" $ do
    it "returns not found if the GCS object doesn't exist" $ do
      Entity _ t <- runDB $ factoryLongDistanceTransfer id
      requestJSON $ do
        setUrl $ LongDistanceTransferR $ longDistanceTransferRecordingUID t
        setMethod "GET"
      statusIs 404

testDelete :: Spec
testDelete = do

  withAppAndMockResponder mockResponderDelete200 $ describe "deleteLongDistanceTransfer (object exists)" $ do
    it "deletes the object from GCS and the database" $ do
      Entity _ t <- runDB $ factoryLongDistanceTransfer id
      requestJSON $ do
        setUrl $ LongDistanceTransferR $ longDistanceTransferRecordingUID t
        setMethod "DELETE"
      statusIs 204

  withAppAndMockResponder mockResponderDelete404 $ describe "deleteLongDistanceTransfer (object doesn't exist)" $ do
    it "succeeds even if the object doesn't exist on GCS, and deletes the database entry" $ do
      Entity _ t <- runDB $ factoryLongDistanceTransfer id
      requestJSON $ do
        setUrl $ LongDistanceTransferR $ longDistanceTransferRecordingUID t
        setMethod "DELETE"
      statusIs 204

mockResponderGet200 :: Request -> Response BSL.ByteString
mockResponderGet200
  Request { host               = "www.googleapis.com"
          , path               = "/storage/v1/b/rumuki/o/recording123"
          , method             = "GET" } =
  Response { responseStatus    = status200
           , responseVersion   = http11
           , responseHeaders   = [("Content-Length", "1")]
           , responseCookieJar = createCookieJar []
           , responseClose'    = ResponseClose $ return ()
           , responseBody      = "{}" } -- In reality an object resource is returned
mockResponderGet200 r = error $ "Unexpected request: " ++ show r

mockResponderGet404 :: Request -> Response BSL.ByteString
mockResponderGet404
  Request { host               = "www.googleapis.com"
          , path               = "/storage/v1/b/rumuki/o/recording123"
          , method             = "GET" } =
  Response { responseStatus    = status404
           , responseVersion   = http11
           , responseHeaders   = [("Content-Length", "0")]
           , responseCookieJar = createCookieJar []
           , responseClose'    = ResponseClose $ return ()
           , responseBody      = "" }
mockResponderGet404 r = error $ "Unexpected request: " ++ show r

mockResponderDelete200 :: Request -> Response BSL.ByteString
mockResponderDelete200
  Request { host               = "www.googleapis.com"
          , path               = "/storage/v1/b/rumuki/o/recording123"
          , method             = "DELETE" } =
  Response { responseStatus    = status200
           , responseVersion   = http11
           , responseHeaders   = [("Content-Length", "0")]
           , responseCookieJar = createCookieJar []
           , responseClose'    = ResponseClose $ return ()
           , responseBody      = "" }
mockResponderDelete200 r = error $ "Unexpected request: " ++ show r

mockResponderDelete404 :: Request -> Response BSL.ByteString
mockResponderDelete404
  Request { host               = "www.googleapis.com"
          , path               = "/storage/v1/b/rumuki/o/recording123"
          , method             = "DELETE" } =
  Response { responseStatus    = status404
           , responseVersion   = http11
           , responseHeaders   = [("Content-Length", "0")]
           , responseCookieJar = createCookieJar []
           , responseClose'    = ResponseClose $ return ()
           , responseBody      = "" }
mockResponderDelete404 r = error $ "Unexpected request: " ++ show r
