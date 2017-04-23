{-# LANGUAGE OverloadedStrings #-}

module LN.Validate.Internal (
  isLowerAlphaNum,
  onlyLowerAlphaNum,
  onlyAlphaNum,
  onlyAlphaNumAndSpaces,
  onlyDisplayNameChars,
  onlyDisplayNamePrint,
  noSpaces,
  isValid,
  isValidApp,
  isValidAppM,
  isValidSafeName,
  isValidEmail,
  isValidNonEmptyString,
  isValidNonEmptyList,
  isValidLength,
  isValidIntRange,
  isValidTags
) where



import           Control.Monad (void)
import           Data.Char     (isAlphaNum, isLower, isSpace, isPrint)
import           Data.Ifte     (teifEither)
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Prelude

import           LN.T.Error    (ApplicationError(..), ValidationError (..), ValidationErrorCode (..))



isLowerAlphaNum :: Char -> Bool
isLowerAlphaNum c = isLower c && isAlphaNum c



onlyLowerAlphaNum :: Text -> Bool
onlyLowerAlphaNum = T.all isLowerAlphaNum



onlyAlphaNum :: Text -> Bool
onlyAlphaNum = T.all isAlphaNum



onlyAlphaNumAndSpaces :: Text -> Bool
onlyAlphaNumAndSpaces = T.all (\c -> isAlphaNum c || isSpace c)



onlyAlphaNumAndHyphens :: Text -> Bool
onlyAlphaNumAndHyphens = T.all (\c -> isAlphaNum c || c == '-')



-- | For users, organizations, and forums
--
onlyDisplayNameChars :: Text -> Bool
onlyDisplayNameChars = T.all (\c -> isAlphaNum c || c == '-' || c == '.' || c == '_' || isSpace c)



-- | For boards, and threads
--
onlyDisplayNamePrint :: Text -> Bool
onlyDisplayNamePrint = T.all isPrint



noSpaces :: Text -> Bool
noSpaces = T.all (not . isSpace)



-- noControlChars :: Text -> Bool
-- noControlChars = T.all (not . isControl)



isValid :: Maybe Text -> Either ValidationErrorCode a -> Either ValidationError a
isValid m_text (Left validation_error_code) = Left $ Validate validation_error_code m_text
isValid _ (Right a)                         = Right a



isValidApp :: Either ValidationError a -> Either ApplicationError a
isValidApp (Left validation_error) = Left $ Error_Validation validation_error
isValidApp (Right a)               = Right a



isValidAppM :: Monad m => Either ValidationError a -> m (Either ApplicationError a)
isValidAppM lr = do
  let lr' = isValidApp lr
  pure lr'



isValidSafeName :: Text -> Either ValidationErrorCode Text
isValidSafeName name = do
  void $ isValidNonEmptyString name
  teifEither name Validate_InvalidCharacters $ onlyLowerAlphaNum name



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



isValidLength :: Int -> Int -> Text -> Either ValidationErrorCode Text
isValidLength min' max' s
  | len < min' = Left Validate_TooShort
  | len > max' = Left Validate_TooLong
  | otherwise                  = Right s
  where
  len = T.length s



isValidIntRange :: Int -> Int -> Int -> Either ValidationErrorCode Int
isValidIntRange value min' max'
  | value < min' = Left Validate_SmallerThanMinimum
  | value > max' = Left Validate_GreaterThanMaximum
  | otherwise                  = Right value



isValidTags :: [Text] -> Either ValidationErrorCode [Text]
isValidTags tags = do
  mapM (\tag -> do
    void $ isValidNonEmptyString tag
    teifEither tag Validate_InvalidCharacters $ onlyAlphaNumAndHyphens tag) tags



isValidNonEmptyList :: Eq a => [a] -> Either ValidationErrorCode [a]
isValidNonEmptyList a = teifEither a Validate_CannotBeEmpty $ a /= []
