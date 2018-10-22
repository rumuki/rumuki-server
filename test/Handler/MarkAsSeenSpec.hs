module Handler.MarkAsSeenSpec (spec) where

import qualified Database.Persist as DB (get)
import           TestImport

spec :: Spec
spec = do

  withApp $ do

    describe "postMarkAsSeenR" $ do

      it "marks the remote transfer as seen" $ do
        device <- makeDevice
        (Entity tid _) <- makeRemoteTransfer device
        makeRequest device
        statusIs 200
        maybeRemoteTransfer <- runDB $ DB.get tid
        assertEq "remote transfer is seen" True $
          isJust (remoteTransferSeen <$> maybeRemoteTransfer)

  where
    recordingUID = "recording123"
    makeDevice = runDB $ retrieve $ factoryDevice id
    makeRemoteTransfer (Entity _ d) = runDB $ factoryRemoteTransfer
      $ \t -> t { remoteTransferRecipientKeyFingerprint = deviceKeyFingerprint d }

    makeRequest (Entity _ d) = requestJSON $ do
      setUrl $ MarkAsSeenR
      setMethod "POST"
      setRequestBody $ encode $ object [
          "recordingUIDs" .= ([recordingUID] :: [Text])
        , "deviceKeyFingerprint" .= deviceKeyFingerprint d ]
