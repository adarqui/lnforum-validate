{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LN.Generate.Internal (
  genUppercaseString,
  genLowercaseString,
  genAlphaString,
  genDigitString,
  genAlphaNumString,
  genSpaceString,
  genPunctChar,
  genAsciiChar,
  genAsciiString,
  genUsername,
  genDisplayNick
) where




import           Test.QuickCheck



oneOf :: forall a. [Gen a] -> Gen a
oneOf = oneof



genUppercaseChar :: Gen Char
genUppercaseChar = elements ['A'..'Z']

genUppercaseString :: Gen String
genUppercaseString = listOf genUppercaseChar



genLowercaseChar :: Gen Char
genLowercaseChar = elements ['a'..'z']

genLowercaseString :: Gen String
genLowercaseString = listOf genLowercaseChar



genAlphaChar :: Gen Char
genAlphaChar = oneOf [genUppercaseChar, genLowercaseChar]

genAlphaString :: Gen String
genAlphaString = listOf genAlphaChar



genDigitChar :: Gen Char
genDigitChar = elements ['0','1','2','3','4','5','6','7','8','9']

genDigitString :: Gen String
genDigitString = listOf genDigitChar



genAlphaNumChar :: Gen Char
genAlphaNumChar = oneOf [genAlphaChar, genDigitChar]

genAlphaNumString :: Gen String
genAlphaNumString = listOf genAlphaNumChar



genSpaceChar :: Gen Char
genSpaceChar = elements [' ']

genSpaceString :: Gen String
genSpaceString = listOf genSpaceChar


genPunctChar :: Gen Char
genPunctChar = elements "!@#$%^&*()_-+=~`{[}]|\\:;\"'<,>.?/"



genAsciiChar :: Gen Char
genAsciiChar = oneOf [genUppercaseChar, genLowercaseChar, genDigitChar, genSpaceChar, genPunctChar]

genAsciiString :: Gen String
genAsciiString = listOf genAsciiChar




genUsername :: Gen String
genUsername = listOf $ oneOf [genUppercaseChar, genLowercaseChar, genDigitChar]



genDisplayNick :: Gen String
genDisplayNick = listOf $ oneOf [genAlphaNumChar, genSpaceChar]




-- genEmail :: IO String
-- genEmail = do
--   user <- generate $ genUsername
--   pure (user <> "@adarq.org")
