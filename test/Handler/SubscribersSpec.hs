module Handler.SubscribersSpec
       (spec) where

import           TestImport

spec :: Spec
spec = withApp $ do

  describe "postSubscribersR" $ do

    it "creates a subscriber" $ do
      makeRequest id
      statusIs 201
      s <- runDB $ selectFirst [SubscriberEmail ==. email] []
      assertEq "Got subscriber" True $ isJust s

    it "throws on invalid email" $ do
      makeRequest $ \_ -> ("notanemail"::Text)
      statusIs 400

    it "ignores duplicates" $ do
      makeRequest id
      statusIs 201
      makeRequest id
      statusIs 201

  where
    email = "test@subscriber.com" :: Text
    makeRequest t = requestJSON $ do
      setMethod "POST"
      setUrl SubscribersR
      setRequestBody $ encode $ object [ "email" .= t email ]
