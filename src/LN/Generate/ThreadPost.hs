{-# LANGUAGE OverloadedStrings #-}

module LN.Generate.ThreadPost (
  genThreadPostTitle,
  genValidThreadPostTitle,
  buildValidThreadPost
) where



import           Data.Either             (isRight)
import           Data.String.Conversions (cs)
import qualified Data.Text               as T (pack)
import           LN.Generate.Internal
import           LN.Sanitize.ThreadPost
import           LN.T.ThreadPost.Request (ThreadPostRequest (..))
import           LN.Validate.ThreadPost
import           Test.QuickCheck



genThreadPostTitle :: Gen (Maybe String)
genThreadPostTitle = genMaybeDescription (maxThreadPostTitle*2)

genValidThreadPostTitle :: Gen (Maybe String)
genValidThreadPostTitle = genMaybeDescription maxThreadPostTitle



buildValidThreadPost :: IO ThreadPostRequest
buildValidThreadPost = do
  org <- go
  if (isRight $ validateThreadPostRequest org)
    then pure org
    else buildValidThreadPost
  where
  go = do
    m_title      <- genIO genValidThreadPostTitle
    body         <- genIO genPostData
    private_tags <- genIO genTags
    tags         <- genIO genTags
    pure $ sanitizeThreadPostRequest $ ThreadPostRequest {
      threadPostRequestTitle       = cs <$> m_title,
      threadPostRequestBody       = body,
      threadPostRequestPrivateTags = map T.pack private_tags,
      threadPostRequestTags        = map T.pack tags,
      threadPostRequestGuard       = 0
    }
