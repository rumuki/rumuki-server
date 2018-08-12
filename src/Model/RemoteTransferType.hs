{-# LANGUAGE TemplateHaskell #-}
module Model.RemoteTransferType where

import           Data.Aeson
import           Database.Persist.TH
import           Prelude

data RemoteTransferType = Video | Album
  deriving (Show, Read, Eq)

derivePersistField "RemoteTransferType"

instance FromJSON RemoteTransferType where
  parseJSON = withText "remote transfer type" $ \case
    "album" -> return Album
    "video" -> return Video
    _ -> fail "remote transfer type must be one of 'album' or 'video'"
