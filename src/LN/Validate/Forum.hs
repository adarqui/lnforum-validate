{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LN.Validate.Forum (
  validateForumRequest,
  isValidForumDisplayName,
  minForumDisplayName,
  maxForumDisplayName,
  maxForumDescription,
  minForumThreadsPerBoard,
  maxForumThreadsPerBoard,
  minForumThreadPostsPerThread,
  maxForumThreadPostsPerThread,
  minForumRecentThreadsLimit,
  maxForumRecentThreadsLimit,
  minForumRecentPostsLimit,
  maxForumRecentPostsLimit,
  minForumMotwLimit,
  maxForumMotwLimit
) where



import           Control.Monad        (void)
import           Data.Ifte            (teifEither)
import           Data.Text            (Text)
import           Prelude

import           LN.Sanitize.Forum    (sanitizeForumRequest)
import           LN.T.Error           (ValidationError (..),
                                       ValidationErrorCode (..))
import           LN.T.Forum           (ForumRequest (..))
import           LN.Validate.Internal



validateForumRequest :: ForumRequest -> Either ValidationError ForumRequest
validateForumRequest org_req = do
  void $ isValid (Just "display_name")             $ isValidForumDisplayName forumRequestDisplayName
  void $ isValid (Just "description")              $ isValidForumDescription forumRequestDescription
  void $ isValid (Just "threads_per_board")        $ isValidIntRange forumRequestThreadsPerBoard minForumThreadsPerBoard maxForumThreadsPerBoard
  void $ isValid (Just "threads_posts_per_thread") $ isValidIntRange forumRequestThreadPostsPerThread minForumThreadPostsPerThread maxForumThreadPostsPerThread
  void $ isValid (Just "recent_threads_limit")     $ isValidIntRange forumRequestRecentThreadsLimit minForumRecentThreadsLimit maxForumRecentThreadsLimit
  void $ isValid (Just "recent_posts_limit")       $ isValidIntRange forumRequestRecentPostsLimit minForumRecentPostsLimit maxForumRecentPostsLimit
  void $ isValid (Just "motw_limit")               $ isValidIntRange forumRequestMotwLimit minForumMotwLimit maxForumMotwLimit
  void $ isValid (Just "tags")                     $ isValidTags forumRequestTags
  Right z
  where
  z@ForumRequest{..} = sanitizeForumRequest org_req



isValidForumDisplayName :: Text -> Either ValidationErrorCode Text
isValidForumDisplayName name = do
  void $ isValidNonEmptyString name
  void $ isValidLength minForumDisplayName maxForumDisplayName name
  teifEither name Validate_InvalidCharacters $ onlyAlphaNumAndSpaces name



isValidForumDescription :: Maybe Text -> Either ValidationErrorCode (Maybe Text)
isValidForumDescription Nothing     = Right Nothing
isValidForumDescription (Just desc) = do
  void $ isValidNonEmptyString desc
  void $ isValidLength 1 maxForumDescription desc
  teifEither (Just desc) Validate_InvalidCharacters $ onlyAlphaNumAndSpaces desc



minForumDisplayName :: Int
minForumDisplayName = 1

maxForumDisplayName :: Int
maxForumDisplayName = 32

maxForumDescription :: Int
maxForumDescription = 512

minForumThreadsPerBoard :: Int
minForumThreadsPerBoard = 5

maxForumThreadsPerBoard :: Int
maxForumThreadsPerBoard = 50

minForumThreadPostsPerThread :: Int
minForumThreadPostsPerThread = 5

maxForumThreadPostsPerThread :: Int
maxForumThreadPostsPerThread = 50

minForumRecentThreadsLimit :: Int
minForumRecentThreadsLimit = 0

maxForumRecentThreadsLimit :: Int
maxForumRecentThreadsLimit = 20

minForumRecentPostsLimit :: Int
minForumRecentPostsLimit = 0

maxForumRecentPostsLimit :: Int
maxForumRecentPostsLimit = 20

minForumMotwLimit :: Int
minForumMotwLimit = 0

maxForumMotwLimit :: Int
maxForumMotwLimit = 20
