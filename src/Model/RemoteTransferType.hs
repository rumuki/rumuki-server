{-# LANGUAGE TemplateHaskell #-}
module Model.RemoteTransferType where

import           Database.Persist.TH
import           Prelude

data RemoteTransferType = Video | Album
  deriving (Show, Read, Eq)

derivePersistField "RemoteTransferType"
