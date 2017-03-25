module Handler.Health where

import           Import

getHealthR :: Handler Html
getHealthR = sendResponseStatus status200 ()

getHealthzR :: Handler Html
getHealthzR = sendResponseStatus status200 ()
