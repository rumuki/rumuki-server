module Handler.RemoteTransferSpec
       (spec) where

import           Data.HashMap.Strict          ((!))
import           Network.HTTP.Client.Internal (Response (..))
import           Network.HTTP.Types           (status404)
import           TestImport

spec :: Spec
spec =  testGet >> testDelete

testGet :: Spec
testGet = do

  let mkResponder = mockGCResponder "/storage/v1/b/rumuki/o/recording123"

  withAppAndMockResponder (mkResponder "GET" id)
    $ describe "getRemoteTransfer (object exists)" $ do

    it "returns the metadata along with an URL to the object" $ do
      Entity _ t <- runDB $ factoryRemoteTransfer id
      requestJSON $ do
        setUrl $ RemoteTransferR $ remoteTransferRecordingUID t
        setMethod "GET"
      statusIs 200
      responseSatisfies "has expected metadata" $ \(Object o) ->
        let (Object r) = o ! "remoteTransfer"
            (String transferType) = r ! "type"
        in transferType == "video"

    it "updates the seen status of the remote transfer" $ do
      Entity _ t <- runDB $ factoryRemoteTransfer id
      requestJSON $ do
        setUrl $ RemoteTransferR $ remoteTransferRecordingUID t
        setMethod "GET"
      unseenTransfers <- runDB $ selectList [RemoteTransferSeen ==. Nothing] []
      assertEq "no unseen transfers" 0 $ length unseenTransfers

  withAppAndMockResponder (mkResponder "GET" $ \r -> r { responseStatus = status404 })
    $ describe "getRemoteTransfer (object doesn't exist)" $ do

    it "returns not found if the GCS object doesn't exist" $ do
      Entity _ t <- runDB $ factoryRemoteTransfer id
      requestJSON $ do
        setUrl $ RemoteTransferR $ remoteTransferRecordingUID t
        setMethod "GET"
      statusIs 404

testDelete :: Spec
testDelete = do

  let mkResponder = mockGCResponder "/storage/v1/b/rumuki/o/recording123"

  withAppAndMockResponder (mkResponder "DELETE" $ \r -> r { responseStatus = status404 })
    $ describe "deleteRemoteTransfer (object exists)" $ do

    it "deletes the object from GCS and the database" $ do
      Entity _ t <- runDB $ factoryRemoteTransfer id
      requestJSON $ do
        setUrl $ RemoteTransferR $ remoteTransferRecordingUID t
        setMethod "DELETE"
      statusIs 204

  withAppAndMockResponder (mkResponder "DELETE" $ \r -> r { responseStatus = status404 })
    $ describe "deleteRemoteTransfer (object doesn't exist)" $ do

    it "succeeds even if the object doesn't exist on GCS, and deletes the database entry" $ do
      Entity _ t <- runDB $ factoryRemoteTransfer id
      requestJSON $ do
        setUrl $ RemoteTransferR $ remoteTransferRecordingUID t
        setMethod "DELETE"
      statusIs 204
