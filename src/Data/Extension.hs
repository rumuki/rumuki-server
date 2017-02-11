module Data.Extension where

import           ClassyPrelude.Yesod
import           Data.Aeson
import qualified Data.ByteString.Base64 as B64

instance FromJSON ByteString where
  parseJSON = withText "base64 string" $ return . B64.decodeLenient . encodeUtf8

instance ToJSON ByteString where
  toJSON = toJSON . decodeUtf8 . B64.encode
