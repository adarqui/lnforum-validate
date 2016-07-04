{-# LANGUAGE OverloadedStrings #-}

module LN.Validate.UserSpec (
  main,
  spec
) where



import qualified Data.Text          as T
import           LN.T.Error
import           LN.T.Internal.JSON ()
import           LN.T.User.Request
import           LN.Validate.User
import           Test.Hspec



main :: IO ()
main = hspec spec



spec :: Spec
spec = do

  describe "validateUserRequest" $ do
    it "validates a user request" $ do
      validateUserRequest test_user_request `shouldBe` Right test_user_request'
      validateUserRequest test_user_request2 `shouldBe` (Left $ Validate Validate_TooLong (Just "display_name"))



test_user_request :: UserRequest
test_user_request =
  UserRequest
    " display  name   "
    "Some  Name "
    "helloworld@email.com "
    "  plugin "
    "ident   "
    Nothing



test_user_request' :: UserRequest
test_user_request' =
  UserRequest
    "display name"
    "Some Name"
    "helloworld@email.com"
    "plugin"
    "ident"
    Nothing



test_user_request2 :: UserRequest
test_user_request2 = test_user_request { userRequestDisplayName = T.replicate 33 "A" }
