{-# LANGUAGE OverloadedStrings #-}

module LN.Validate.UserSpec (
  main,
  spec
) where



import           LN.Validate.User
import           LN.T.Internal.JSON ()
import           LN.T.User.Request
import           Test.Hspec



main :: IO ()
main = hspec spec



spec :: Spec
spec = do

  describe "validateUserRequest" $ do
    it "validates a user request" $ do
      validateUserRequest test_user_request `shouldBe` Right test_user_request'



test_user_request :: UserRequest
test_user_request =
  UserRequest
    " display  nick   "
    "Some  Name "
    "helloworld@email.com "
    "  plugin "
    "ident   "
    Nothing



test_user_request' :: UserRequest
test_user_request' =
  UserRequest
    "display nick"
    "Some Name"
    "helloworld@email.com"
    "plugin"
    "ident"
    Nothing
