{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LN.Validate.ThreadPost (
  validateThreadPostRequest,
  isValidThreadPostTitle,
  minThreadPostTitle,
  maxThreadPostTitle,
) where



import           Control.Monad           (void)
import           Data.Ifte               (teifEither)
import           Data.Text               (Text)
import           LN.Sanitize.ThreadPost  (sanitizeThreadPostRequest)
import           LN.T.Error              (ValidationError (..),
                                          ValidationErrorCode (..))
import           LN.T.ThreadPost.Request (ThreadPostRequest (..))
import           LN.Validate.Internal



validateThreadPostRequest :: ThreadPostRequest -> Either ValidationError ThreadPostRequest
validateThreadPostRequest org_req = do
  void $ isValid (Just "title")        $ isValidThreadPostTitle threadPostRequestTitle
  void $ isValid (Just "tags")         $ isValidTags threadPostRequestTags
  void $ isValid (Just "private_tags") $ isValidTags threadPostRequestPrivateTags
  Right z
  where
  z@ThreadPostRequest{..} = sanitizeThreadPostRequest org_req



isValidThreadPostTitle :: Maybe Text -> Either ValidationErrorCode (Maybe Text)
isValidThreadPostTitle Nothing      = Right Nothing
isValidThreadPostTitle (Just title) = do
  void $ isValidNonEmptyString title
  void $ isValidLength minThreadPostTitle maxThreadPostTitle title
  teifEither (Just title) Validate_InvalidCharacters $ onlyAlphaNumAndSpaces title



minThreadPostTitle :: Int
minThreadPostTitle = 1

maxThreadPostTitle :: Int
maxThreadPostTitle = 132
