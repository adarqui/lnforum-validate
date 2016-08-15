{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LN.Validate.User (
  validateUserRequest,
  isValidUserDisplayName,
  minUserDisplayName,
  maxUserDisplayName,
  minUserFullName,
  maxUserFullName,
  minUserName,
  maxUserName
) where



import           Control.Monad        (void)
import           Data.Ifte            (teifEither)
import           Data.Text            (Text)
import           Prelude

import           LN.Sanitize.User     (sanitizeUserRequest)
import           LN.T.Error           (ValidationError (..),
                                       ValidationErrorCode (..))
import           LN.T.User            (UserRequest (..))
import           LN.Validate.Internal



validateUserRequest :: UserRequest -> Either ValidationError UserRequest
validateUserRequest user_req = do
  void $ isValid (Just "display_name") $ isValidUserDisplayName userRequestDisplayName
  void $ isValid (Just "full_name")    $ isValidUserFullName userRequestFullName
  void $ isValid (Just "email")        $ isValidEmail userRequestEmail
  void $ isValid (Just "plugin")       $ isValidNonEmptyString userRequestPlugin
  void $ isValid (Just "ident")        $ isValidNonEmptyString userRequestIdent
  Right z
  where
  z@UserRequest{..} = sanitizeUserRequest user_req



isValidUserDisplayName :: Text -> Either ValidationErrorCode Text
isValidUserDisplayName name = do
  void $ isValidNonEmptyString name
  void $ isValidLength minUserDisplayName maxUserDisplayName name
  teifEither name Validate_InvalidCharacters $ onlyDisplayNameChars name



isValidUserFullName :: Text -> Either ValidationErrorCode Text
isValidUserFullName = isValidUserDisplayName



minUserDisplayName :: Int
minUserDisplayName = 1

maxUserDisplayName :: Int
maxUserDisplayName = 32

minUserFullName :: Int
minUserFullName = minUserDisplayName

maxUserFullName :: Int
maxUserFullName = maxUserDisplayName

minUserName :: Int
minUserName = 2

maxUserName :: Int
maxUserName = 32
