{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LN.Validate.Board (
  validateBoardRequest,
  isValidBoardDisplayName,
  minBoardDisplayName,
  maxBoardDisplayName,
  maxBoardDescription,
) where



import           Control.Monad        (void)
import           Data.Ifte            (teifEither)
import           Data.Text            (Text)
import           LN.Sanitize.Board    (sanitizeBoardRequest)
import           LN.T.Board           (BoardRequest (..))
import           LN.T.Error           (ValidationError (..),
                                       ValidationErrorCode (..))
import           LN.Validate.Internal



validateBoardRequest :: BoardRequest -> Either ValidationError BoardRequest
validateBoardRequest org_req = do
  void $ isValid (Just "display_name")   $ isValidBoardDisplayName boardRequestDisplayName
  void $ isValid (Just "description")    $ isValidBoardDescription boardRequestDescription
  void $ isValid (Just "suggested_tags") $ isValidTags boardRequestSuggestedTags
  void $ isValid (Just "tags")           $ isValidTags boardRequestTags
  Right z
  where
  z@BoardRequest{..} = sanitizeBoardRequest org_req



isValidBoardDisplayName :: Text -> Either ValidationErrorCode Text
isValidBoardDisplayName name = do
  void $ isValidNonEmptyString name
  void $ isValidLength minBoardDisplayName maxBoardDisplayName name
  teifEither name Validate_InvalidCharacters $ onlyAlphaNumAndSpaces name



isValidBoardDescription :: Maybe Text -> Either ValidationErrorCode (Maybe Text)
isValidBoardDescription Nothing     = Right Nothing
isValidBoardDescription (Just desc) = do
  void $ isValidNonEmptyString desc
  void $ isValidLength 1 maxBoardDescription desc
  teifEither (Just desc) Validate_InvalidCharacters $ onlyAlphaNumAndSpaces desc



minBoardDisplayName :: Int
minBoardDisplayName = 1

maxBoardDisplayName :: Int
maxBoardDisplayName = 32

maxBoardDescription :: Int
maxBoardDescription = 132
