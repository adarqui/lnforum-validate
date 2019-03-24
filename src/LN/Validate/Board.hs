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
import           Prelude

import           LN.Sanitize.Board    (sanitizeBoardRequest)
import           LN.T.Board           (BoardRequest (..))
import           LN.T.Error           (ValidationError (..),
                                       ValidationErrorCode (..))
import           LN.Validate.Internal



validateBoardRequest :: BoardRequest -> Either ValidationError BoardRequest
validateBoardRequest req = do
  void $ isValid (Just "display_name")   $ isValidBoardDisplayName boardRequestDisplayName
  void $ isValid (Just "description")    $ isValidBoardDescription boardRequestDescription
  void $ isValid (Just "tags")           $ isValidTags boardRequestTags
  Right z
  where
  z@BoardRequest{..} = sanitizeBoardRequest req



isValidBoardDisplayName :: Text -> Either ValidationErrorCode Text
isValidBoardDisplayName name = do
  void $ isValidNonEmptyString name
  void $ isValidLength minBoardDisplayName maxBoardDisplayName name
  teifEither name Validate_InvalidCharacters $ onlyDisplayNamePrint name



isValidBoardDescription :: Maybe Text -> Either ValidationErrorCode (Maybe Text)
isValidBoardDescription Nothing     = Right Nothing
isValidBoardDescription (Just desc) = do
  void $ isValidNonEmptyString desc
  void $ isValidLength 1 maxBoardDescription desc
  teifEither (Just desc) Validate_InvalidCharacters $ onlyDisplayNamePrint desc



minBoardDisplayName :: Int
minBoardDisplayName = 1

maxBoardDisplayName :: Int
maxBoardDisplayName = 64

maxBoardDescription :: Int
maxBoardDescription = 132
