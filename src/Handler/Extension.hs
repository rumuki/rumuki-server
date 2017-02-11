-- | Some tools for handlers
module Handler.Extension
       ( requireJsonEntity
       , parseJsonEntity
       , noWhitelist
       , whitelist
       , fromMaybeM
       ) where

import           Data.Aeson              as A
import           Data.Aeson.Parser       as AP
import           Data.Aeson.Types        (Pair)
import           Data.Conduit.Attoparsec (sinkParser)
import           Data.Conduit.Lift
import           Data.HashMap.Strict     as H
import           Import.NoFoundation

-- | Syntatic sugar, alias for @Just@
whitelist :: [Text] -> Maybe [Text]
whitelist = Just

-- | Syntatic sugar, alias for @Nothing@
noWhitelist :: Maybe [Text]
noWhitelist = Nothing

-- | Retrieve an entity from the request body.
--   Using the given @Pair@s as defaults for missing values.
requireJsonEntity :: (MonadHandler m, FromJSON e)
                  => [Pair]       -- ^ Default values injected into the JSON
                  -> Maybe [Text] -- ^ A whitelist of allowed keys
                  -> m e
requireJsonEntity defs wl = do
  eValue <- rawRequestBody $$ runCatchC (sinkParser AP.value')
  either (invalidArgs . (: []) . pack . show) (parseJsonEntity defs wl) eValue

-- | Given a JSON @Value@, parse it and try to create the appropiate type that
--   conforms to @FromJSON@
parseJsonEntity :: (MonadHandler m, FromJSON e)
                => [Pair] -- ^ Default values for the given JSON
                -> Maybe [Text] -- ^ White-list of allowed keys
                -> Value -- ^ JSON
                -> m e
parseJsonEntity defs wl v =
  let filterf k _ = maybe True (k `elem`) wl
      dofilter    = H.filterWithKey filterf
      result      = case v of
        (Object o) -> fromJSON (Object $ dofilter o `H.union` H.fromList defs)
        o          -> fromJSON o
  in case result of Error s   -> invalidArgs [pack s]
                    Success a -> return a

-- | Unwrap a maybe wrapped in a monad. Useful for getting
--   I/O results whilst fallbacking to a response/value
--   This differs from @liftM2 fromMaybe@ because it's lazy.
fromMaybeM :: Monad m =>  m a -> m (Maybe a) -> m a
fromMaybeM e h = do
  h' <- h
  case h' of
    Just a -> return a
    Nothing -> e
