{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LN.Validate.User (
  validateUserRequest
) where



import           Control.Monad        (void)
import           LN.Sanitize.User     (sanitizeUserRequest)
import           LN.T.Error           (ValidationError)
import           LN.T.User.Request    (UserRequest (..))
import           LN.Validate.Internal



validateUserRequest :: UserRequest -> Either ValidationError UserRequest
validateUserRequest user_req = do
  void $ isValid (Just "display_nick") $ isValidDisplayName userRequestDisplayNick
  void $ isValid (Just "name")         $ isValidDisplayName userRequestName
  void $ isValid (Just "email")        $ isValidEmail userRequestEmail
  void $ isValid (Just "plugin")       $ isValidNonEmptyString userRequestPlugin
  void $ isValid (Just "ident")        $ isValidNonEmptyString userRequestIdent
  Right z
  where
  z@UserRequest{..} = sanitizeUserRequest user_req
