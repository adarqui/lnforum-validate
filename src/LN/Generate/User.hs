{-# LANGUAGE OverloadedStrings #-}

module LN.Generate.User (
  buildValidUser
) where



import           Control.Monad.IO.Class  (liftIO)
import           Data.Monoid             ((<>))
import           Data.String.Conversions (cs)
import           LN.Generate.Internal    (genDisplayNick)
import           LN.T.User.Request       (UserRequest (..))
import           Test.QuickCheck         (generate)



buildValidUser :: IO UserRequest
buildValidUser = do
  display_nick <- liftIO $ generate genDisplayNick
  let nick     =  filter (/= ' ') display_nick
  pure $ UserRequest {
    userRequestDisplayNick = cs display_nick,
    userRequestName        = cs display_nick,
    userRequestEmail       = cs $ nick <> "@adarq.org",
    userRequestPlugin      = "ln-validate",
    userRequestIdent       = cs nick,
    userRequestAcceptTOS   = Nothing
  }
