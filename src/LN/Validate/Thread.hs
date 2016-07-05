{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LN.Validate.Thread (
  validateThreadRequest,
  isValidThreadDisplayName,
  minThreadDisplayName,
  maxThreadDisplayName,
  maxThreadDescription,
) where



import           Control.Monad        (void)
import           Data.Ifte            (teifEither)
import           Data.Text            (Text)
import           LN.Sanitize.Thread   (sanitizeThreadRequest)
import           LN.T.Error           (ValidationError (..),
                                       ValidationErrorCode (..))
import           LN.T.Thread.Request  (ThreadRequest (..))
import           LN.Validate.Internal



validateThreadRequest :: ThreadRequest -> Either ValidationError ThreadRequest
validateThreadRequest org_req = do
  void $ isValid (Just "display_name")   $ isValidThreadDisplayName threadRequestDisplayName
  void $ isValid (Just "description")    $ isValidThreadDescription threadRequestDescription
  void $ isValid (Just "tags")           $ isValidTags threadRequestTags
  Right z
  where
  z@ThreadRequest{..} = sanitizeThreadRequest org_req



isValidThreadDisplayName :: Text -> Either ValidationErrorCode Text
isValidThreadDisplayName name = do
  void $ isValidNonEmptyString name
  void $ isValidLength minThreadDisplayName maxThreadDisplayName name
  teifEither name Validate_InvalidCharacters $ onlyAlphaNumAndSpaces name



isValidThreadDescription :: Maybe Text -> Either ValidationErrorCode (Maybe Text)
isValidThreadDescription Nothing     = Right Nothing
isValidThreadDescription (Just desc) = do
  void $ isValidNonEmptyString desc
  void $ isValidLength 1 maxThreadDescription desc
  teifEither (Just desc) Validate_InvalidCharacters $ onlyAlphaNumAndSpaces desc



minThreadDisplayName :: Int
minThreadDisplayName = 1

maxThreadDisplayName :: Int
maxThreadDisplayName = 32

maxThreadDescription :: Int
maxThreadDescription = 132
