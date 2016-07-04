{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LN.Generate.Internal (
  oneOf,
  genUppercaseChar,
  genUppercaseString,
  genLowercaseChar,
  genLowercaseString,
  genAlphaChar,
  genAlphaString,
  genDigitChar,
  genDigitString,
  genAlphaNumChar,
  genAlphaNumString,
  genSpaceChar,
  genSpaceString,
  genPunctChar,
  genAsciiChar,
  genAsciiString,
  genDisplayName'1,
  genMaybeDescription,
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



genDisplayName'1 :: Int -> Int -> Gen String
genDisplayName'1 i j = choose (i, j) >>= flip vectorOf (oneOf [genAlphaNumChar, genSpaceChar])



genMaybeDescription :: Int -> Gen (Maybe String)
genMaybeDescription j = do
  sz <- choose (False, True)
  if sz
    then Just <$> vectorOf j (oneOf [genAsciiChar])
    else pure $ Nothing
