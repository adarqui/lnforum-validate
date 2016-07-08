{-# LANGUAGE OverloadedStrings #-}

module LN.Generate.User (
  genUserDisplayName,
  genValidUserDisplayName,
  genUserFullName,
  genValidUserFullName,
  buildValidUser
) where



import           Data.Either             (isRight)
import           Data.Monoid             ((<>))
import           Data.String.Conversions (cs)
import           LN.Generate.Internal
import           LN.Sanitize.User
import           LN.T.User               (UserRequest (..))
import           LN.Validate.User
import           Test.QuickCheck



genUserDisplayName :: Gen String
genUserDisplayName = genDisplayName'1 0 100

genValidUserDisplayName :: Gen String
genValidUserDisplayName = genDisplayName'1 10 maxUserDisplayName



genUserFullName :: Gen String
genUserFullName = genUserDisplayName

genValidUserFullName :: Gen String
genValidUserFullName = genValidUserDisplayName



buildValidUser :: IO UserRequest
buildValidUser = do
  user <- go
  if (isRight $ validateUserRequest user)
    then pure user
    else buildValidUser
  where
  go = do
    display_name <- genIO genValidUserDisplayName
    full_name    <- genIO genValidUserFullName
    let name     =  filter (/= ' ') display_name
    pure $ sanitizeUserRequest $ UserRequest {
      userRequestDisplayName = cs display_name,
      userRequestFullName    = cs full_name,
      userRequestEmail       = cs $ name <> "@adarq.org",
      userRequestPlugin      = "ln-validate",
      userRequestIdent       = cs name,
      userRequestAcceptTOS   = Nothing
    }
