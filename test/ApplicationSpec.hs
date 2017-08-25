module ApplicationSpec (spec) where

import           Application
import qualified Data.ByteString.Lazy         as LB
import           Data.Time.Clock              (addUTCTime, getCurrentTime)
import qualified Database.Persist             as DB
import           Model
import qualified Network.HTTP.Client.Internal as HTTP
import qualified Network.HTTP.Types           as HTTP
import           TestFactory
import           TestImport

spec :: Spec
spec = do

  let mkResponder = mockGCResponder "/storage/v1/b/rumuki/o/recording123"

  describe "application :: removeStaleObjects" $ do

    withAppAndMockResponder (
      mkResponder "DELETE"
      $ setResponseStatus HTTP.status204) $ do

      it "removes expired playback grants" $ do
        now <- liftIO getCurrentTime
        app <- getTestYesod
        device <- runDB . retrieve $ factoryDevice id
        (Entity gid _) <- runDB $ factoryPlaybackGrant device
                          $ \g -> g { playbackGrantExpires = now }
        maybeGrant <- runDB $ DB.get gid
        assertEq "Playback grant exists" True (isJust maybeGrant)
        liftIO $ removeStaleObjects app
        maybeGrant' <- runDB $ DB.get gid
        assertEq "Playback grant removed" False (isJust maybeGrant')

      it "doesn't remove non-expired playback grants" $ do
        now <- liftIO getCurrentTime
        app <- getTestYesod
        device <- runDB . retrieve $ factoryDevice id
        (Entity gid _) <- runDB $ factoryPlaybackGrant device
                          $ \g -> g { playbackGrantExpires = addUTCTime 60 now }
        maybeGrant <- runDB $ DB.get gid
        assertEq "Playback grant exists" True (isJust maybeGrant)
        liftIO $ removeStaleObjects app
        maybeGrant' <- runDB $ DB.get gid
        assertEq "Playback grant exists" True (isJust maybeGrant')

      it "removes seen and stale remote transfers" $ do
        now <- liftIO getCurrentTime
        app <- getTestYesod
        (Entity tid _) <- runDB $ factoryRemoteTransfer $ \t ->
          t { remoteTransferSeen = Just $ addUTCTime (-1 * 60 * 60 * 24 * 30) now }
        maybeTransfer <- runDB $ DB.get tid
        assertEq "Transfer exists" True (isJust maybeTransfer)
        liftIO $ removeStaleObjects app
        maybeTransfer' <- runDB $ DB.get tid
        assertEq "Transfer doesn't exist" False (isJust maybeTransfer')

      it "doesn't remove unseen remote transfers" $ do
        app <- getTestYesod
        (Entity tid _) <- runDB $ factoryRemoteTransfer $ \t -> t { remoteTransferSeen = Nothing}
        maybeTransfer <- runDB $ DB.get tid
        assertEq "Transfer exists" True (isJust maybeTransfer)
        liftIO $ removeStaleObjects app
        maybeTransfer' <- runDB $ DB.get tid
        assertEq "Transfer exists" True (isJust maybeTransfer')

      it "doesn't remove recently seen remote transfers" $ do
        now <- liftIO getCurrentTime
        app <- getTestYesod
        (Entity tid _) <- runDB $ factoryRemoteTransfer $ \t -> t {
          remoteTransferSeen = Just $ addUTCTime (-1 * 60 * 60) now }
        maybeTransfer <- runDB $ DB.get tid
        assertEq "Transfer exists" True (isJust maybeTransfer)
        liftIO $ removeStaleObjects app
        maybeTransfer' <- runDB $ DB.get tid
        assertEq "Transfer exists" True (isJust maybeTransfer')

    withAppAndMockResponder (
      mkResponder "DELETE"
      $ setResponseStatus HTTP.status404) $ do

      it "doesn't remove a remote transfer if the GCS delete failed" $ do
        now <- liftIO getCurrentTime
        app <- getTestYesod
        (Entity tid _) <- runDB $ factoryRemoteTransfer $ \t ->
          t { remoteTransferSeen = Just $ addUTCTime (-1 * 60 * 60 * 24 * 30) now }
        maybeTransfer <- runDB $ DB.get tid
        assertEq "Transfer exists" True (isJust maybeTransfer)
        liftIO $ removeStaleObjects app
        maybeTransfer' <- runDB $ DB.get tid
        assertEq "Transfer exists" True (isJust maybeTransfer')

setResponseStatus :: HTTP.Status ->
                     HTTP.Response LB.ByteString ->
                     HTTP.Response LB.ByteString
setResponseStatus status req = req { HTTP.responseStatus = status }
