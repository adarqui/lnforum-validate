{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LN.Validate.User (
  validateUserRequest
) where



import           Control.Monad        (void)
import           LN.T.Error           (ValidationError)
import           LN.T.User.Request    (UserRequest (..))
import           LN.Validate.Internal



validateUserRequest :: UserRequest -> Either ValidationError UserRequest
validateUserRequest z@UserRequest{..} = do
  void $ invalid (Just "display_nick") $ isValidName userRequestDisplayNick
  void $ invalid (Just "name")         $ isValidName userRequestName
  void $ invalid (Just "email")        $ isValidEmail userRequestEmail
  void $ invalid (Just "plugin")       $ isValidNonEmptyString userRequestPlugin
  void $ invalid (Just "ident")        $ isValidNonEmptyString userRequestIdent
  Right z
