{-# LANGUAGE OverloadedStrings #-}

module LN.Generate.User (
  genUserDisplayName,
  genValidUserDisplayName,
  genUserFullName,
  genValidUserFullName,
  genUserFullName,
  genValidUserFullName,
  buildValidUser
) where



import           Control.Monad.IO.Class  (liftIO)
import           Data.Monoid             ((<>))
import           Data.String.Conversions (cs)
import           LN.Generate.Internal
import           LN.Generate.Internal
import           LN.T.User.Request       (UserRequest (..))
import           LN.Validate.User
import           Test.QuickCheck



genUserDisplayName :: Gen String
genUserDisplayName = genDisplayName'1 0 100

genValidUserDisplayName :: Gen String
genValidUserDisplayName = genDisplayName'1 minUserDisplayName maxUserDisplayName



genUserFullName :: Gen String
genUserFullName = genUserDisplayName

genValidUserFullName :: Gen String
genValidUserFullName = genValidUserDisplayName



buildValidUser :: IO UserRequest
buildValidUser = do
  display_name <- liftIO $ generate genValidUserDisplayName
  full_name    <- liftIO $ generate genValidUserFullName
  let name     =  filter (/= ' ') display_name
  pure $ UserRequest {
    userRequestDisplayName = cs display_name,
    userRequestFullName    = cs full_name,
    userRequestEmail       = cs $ name <> "@adarq.org",
    userRequestPlugin      = "ln-validate",
    userRequestIdent       = cs name,
    userRequestAcceptTOS   = Nothing
  }
