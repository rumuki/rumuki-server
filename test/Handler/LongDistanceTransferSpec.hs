module Handler.LongDistanceTransferSpec
       (spec) where

import           Network.HTTP.Client.Internal (Response (..))
import           Network.HTTP.Types           (status404)
import           TestImport

spec :: Spec
spec =  testGet >> testDelete

testGet :: Spec
testGet = do

  let mkResponder = mockGCResponder "/storage/v1/b/rumuki/o/recording123"

  withAppAndMockResponder (mkResponder "GET" id)
    $ describe "getLongDistanceTransfer (object exists)" $ do

    it "returns the metadata along with an URL to the object" $ do
      Entity _ t <- runDB $ factoryLongDistanceTransfer id
      requestJSON $ do
        setUrl $ LongDistanceTransferR $ longDistanceTransferRecordingUID t
        setMethod "GET"
      statusIs 200

  withAppAndMockResponder (mkResponder "GET" $ \r -> r { responseStatus = status404 })
    $ describe "getLongDistanceTransfer (object doesn't exist)" $ do

    it "returns not found if the GCS object doesn't exist" $ do
      Entity _ t <- runDB $ factoryLongDistanceTransfer id
      requestJSON $ do
        setUrl $ LongDistanceTransferR $ longDistanceTransferRecordingUID t
        setMethod "GET"
      statusIs 404

testDelete :: Spec
testDelete = do

  let mkResponder = mockGCResponder "/storage/v1/b/rumuki/o/recording123"

  withAppAndMockResponder (mkResponder "DELETE" $ \r -> r { responseStatus = status404 })
    $ describe "deleteLongDistanceTransfer (object exists)" $ do

    it "deletes the object from GCS and the database" $ do
      Entity _ t <- runDB $ factoryLongDistanceTransfer id
      requestJSON $ do
        setUrl $ LongDistanceTransferR $ longDistanceTransferRecordingUID t
        setMethod "DELETE"
      statusIs 204

  withAppAndMockResponder (mkResponder "DELETE" $ \r -> r { responseStatus = status404 })
    $ describe "deleteLongDistanceTransfer (object doesn't exist)" $ do

    it "succeeds even if the object doesn't exist on GCS, and deletes the database entry" $ do
      Entity _ t <- runDB $ factoryLongDistanceTransfer id
      requestJSON $ do
        setUrl $ LongDistanceTransferR $ longDistanceTransferRecordingUID t
        setMethod "DELETE"
      statusIs 204
