{-# LANGUAGE OverloadedStrings #-}

module LN.Generate.Forum (
  buildValidForum,
  genForumDisplayName,
  genValidForumDisplayName,
  genForumDescription,
  genValidForumDescription,
  genForumThreadsPerBoard,
  genValidForumThreadsPerBoard,
  genForumThreadPostsPerThread,
  genValidForumThreadPostsPerThread,
  genForumRecentThreadsLimit,
  genValidForumRecentThreadsLimit,
  genForumRecentPostsLimit,
  genValidForumRecentPostsLimit,
  genForumMotwLimit,
  genValidForumMotwLimit
) where



import           Control.Monad.IO.Class  (liftIO)
import           Data.Either             (isRight)
import           Data.String.Conversions (cs)
import qualified Data.Text               as T (pack)
import           LN.Generate.Internal
import           LN.Sanitize.Forum
import           LN.T.Forum.Request      (ForumRequest (..))
import           LN.Validate.Forum
import           Test.QuickCheck



buildValidForum :: IO ForumRequest
buildValidForum = do
  forum <- go
  if (isRight $ validateForumRequest forum)
    then pure forum
    else buildValidForum
  where
  go = do
    display_name            <- liftIO $ generate genValidForumDisplayName
    m_desc                  <- liftIO $ generate genValidForumDescription
    threads_per_board       <- liftIO $ generate genValidForumThreadsPerBoard
    thread_posts_per_thread <- liftIO $ generate genValidForumThreadPostsPerThread
    recent_threads_limit    <- liftIO $ generate genValidForumRecentThreadsLimit
    recent_posts_limit      <- liftIO $ generate genValidForumRecentPostsLimit
    motw_limit              <- liftIO $ generate genValidForumMotwLimit
    forum_tags              <- liftIO $ generate genTags
    visibility              <- liftIO $ generate genVisibility
    pure $ sanitizeForumRequest $ ForumRequest {
      forumRequestDisplayName          = cs display_name,
      forumRequestDescription          = fmap cs m_desc,
      forumRequestThreadsPerBoard      = threads_per_board,
      forumRequestThreadPostsPerThread = thread_posts_per_thread,
      forumRequestRecentThreadsLimit   = recent_threads_limit,
      forumRequestRecentPostsLimit     = recent_posts_limit,
      forumRequestMotwLimit            = motw_limit,
      forumRequestIcon                 = Nothing,
      forumRequestTags                 = map T.pack forum_tags,
      forumRequestVisibility           = visibility,
      forumRequestGuard                = 0
    }



genForumDisplayName :: Gen String
genForumDisplayName = genDisplayName'1 0 100

genValidForumDisplayName :: Gen String
genValidForumDisplayName = genDisplayName'1 10 maxForumDisplayName



genForumDescription :: Gen (Maybe String)
genForumDescription = genMaybeDescription (maxForumDescription*2)

genValidForumDescription :: Gen (Maybe String)
genValidForumDescription = genMaybeDescription maxForumDescription



genForumThreadsPerBoard :: Gen Int
genForumThreadsPerBoard = genIntRange 0 (maxForumThreadsPerBoard*2)

genValidForumThreadsPerBoard :: Gen Int
genValidForumThreadsPerBoard = genIntRange minForumThreadsPerBoard maxForumThreadsPerBoard



genForumThreadPostsPerThread :: Gen Int
genForumThreadPostsPerThread = genIntRange 0 (maxForumThreadPostsPerThread*2)

genValidForumThreadPostsPerThread :: Gen Int
genValidForumThreadPostsPerThread = genIntRange minForumThreadPostsPerThread maxForumThreadPostsPerThread



genForumRecentThreadsLimit :: Gen Int
genForumRecentThreadsLimit = genIntRange 0 (maxForumRecentThreadsLimit*2)

genValidForumRecentThreadsLimit :: Gen Int
genValidForumRecentThreadsLimit = genIntRange minForumRecentThreadsLimit maxForumRecentThreadsLimit



genForumRecentPostsLimit :: Gen Int
genForumRecentPostsLimit = genIntRange 0 (maxForumRecentPostsLimit*2)

genValidForumRecentPostsLimit :: Gen Int
genValidForumRecentPostsLimit = genIntRange minForumRecentPostsLimit maxForumRecentPostsLimit



genForumMotwLimit :: Gen Int
genForumMotwLimit = genIntRange 0 (maxForumMotwLimit*2)

genValidForumMotwLimit :: Gen Int
genValidForumMotwLimit = genIntRange minForumMotwLimit maxForumMotwLimit
