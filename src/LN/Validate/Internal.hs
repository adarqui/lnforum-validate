{-# LANGUAGE OverloadedStrings #-}

module LN.Validate.Internal (
  isLowerAlphaNum,
  invalid,
  isValidNick,
  isValidDisplayName,
  isValidName,
  isValidEmail,
  isValidNonEmptyString
) where



import           Control.Monad (void)
import           Data.Char     (isAlphaNum, isLower, isSpace)
import           Data.Ifte     (teifEither)
import           Data.Text     (Text)
import qualified Data.Text     as T
import           LN.T.Error    (ValidationError (..), ValidationErrorCode (..))



isLowerAlphaNum :: Char -> Bool
isLowerAlphaNum c = isLower c && isAlphaNum c



onlyLowerAlphaNum :: Text -> Bool
onlyLowerAlphaNum = T.all isLowerAlphaNum



onlyAlphaNum :: Text -> Bool
onlyAlphaNum = T.all isAlphaNum



noSpaces :: Text -> Bool
noSpaces = T.all (not . isSpace)



-- noControlChars :: Text -> Bool
-- noControlChars = T.all (not . isControl)



invalid :: Maybe Text -> Either ValidationErrorCode a -> Either ValidationError a
invalid m_text (Left validation_error_code) = Left $ Validate validation_error_code m_text
invalid _ (Right a)                         = Right a



isValidNick :: Text -> Either ValidationErrorCode Text
isValidNick nick = do
  void $ isValidNonEmptyString nick
  teifEither nick Validate_InvalidCharacters $ onlyLowerAlphaNum nick



isValidDisplayName :: Text -> Either ValidationErrorCode Text
isValidDisplayName name = do
  void $ isValidNonEmptyString name
  teifEither name Validate_InvalidCharacters $ onlyAlphaNum name



isValidName :: Text -> Either ValidationErrorCode Text
isValidName name = do
  void $ isValidNonEmptyString name
  teifEither name Validate_InvalidCharacters $ onlyAlphaNum name



isValidEmail :: Text -> Either ValidationErrorCode Text
isValidEmail email = do
  void $ isValidNonEmptyString email
  void $ teifEither email Validate_InvalidEmail $ noSpaces email
  void $ teifEither email Validate_InvalidEmail $ (T.length $ T.filter (=='@') email) == 1
  void $ teifEither email Validate_InvalidEmail $ name /= "" && domain /= "" && tld /= "."
  Right email
  where
  (name, rest) = T.breakOn "@" email
  (domain, tld) = T.breakOn "." rest



isValidNonEmptyString :: Text -> Either ValidationErrorCode Text
isValidNonEmptyString s = teifEither s Validate_CannotBeEmpty $ s /= ""
