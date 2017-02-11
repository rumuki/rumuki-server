module Model.Extension
       ( RequestView (..)
       , ResponseView (..) ) where

newtype RequestView e = RequestView e
newtype ResponseView e = ResponseView e
