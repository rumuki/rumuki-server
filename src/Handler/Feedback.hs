module Handler.Feedback where

import           Import

postFeedbackR :: Handler Value
postFeedbackR = do
  now <- liftIO getCurrentTime
  feedback' <- requireJsonEntity [] Nothing :: Handler Feedback
  let feedback = feedback' { feedbackCreated = Just now }
  _ <- runDB $ insert feedback
  sendResponseStatus status201 $ object [ "feedback" .= feedback ]
