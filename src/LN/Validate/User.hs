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
  void $ invalid (Just "display_nick") $ isValidDisplayName userRequestDisplayNick
  void $ invalid (Just "name")         $ isValidDisplayName userRequestName
  void $ invalid (Just "email")        $ isValidEmail userRequestEmail
  void $ invalid (Just "plugin")       $ isValidNonEmptyString userRequestPlugin
  void $ invalid (Just "ident")        $ isValidNonEmptyString userRequestIdent
  Right z
  where
  z@UserRequest{..} = sanitizeUserRequest user_req
