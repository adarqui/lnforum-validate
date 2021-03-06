{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LN.Generate.Internal (
  genIO,
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
  genIntRange,
  genDisplayName'1,
  genMaybeDescription,
  genVisibility,
  genProfileGender,
  genTags,
  genBool,
  genPostDataRaw,
  genPostDataBBCode,
  genPostData
) where




import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text              as T (pack)
import           Prelude
import           Test.QuickCheck

import           LN.T.Profile           (ProfileGender (..))
import           LN.T.ThreadPost        (PostData (..))
import           LN.T.Visibility        (Visibility (..))



genIO :: forall a. Gen a -> IO a
genIO = liftIO . generate



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



genHyphenChar :: Gen Char
genHyphenChar = elements ['-']



genPunctChar :: Gen Char
genPunctChar = elements "!@#$%^&*()_-+=~`{[}]|\\:;\"'<,>.?/"



genAsciiChar :: Gen Char
genAsciiChar = oneOf [genUppercaseChar, genLowercaseChar, genDigitChar, genSpaceChar, genPunctChar]

genAsciiString :: Gen String
genAsciiString = listOf genAsciiChar



genIntRange :: Int -> Int -> Gen Int
genIntRange i j = choose (i, j)



genDisplayName'1 :: Int -> Int -> Gen String
genDisplayName'1 i j = choose (i, j) >>= flip vectorOf (oneOf [genAlphaNumChar, genSpaceChar])



genMaybeDescription :: Int -> Gen (Maybe String)
genMaybeDescription j = do
  sz <- choose (False, True)
  if sz
    then Just <$> vectorOf j (oneOf [genAsciiChar])
    else pure $ Nothing



genVisibility :: Gen Visibility
genVisibility = elements [Public, Private]



genProfileGender :: Gen ProfileGender
genProfileGender = elements [GenderMale, GenderFemale, GenderUnknown]



genTagChar :: Gen Char
genTagChar = oneOf [genAlphaNumChar, genHyphenChar]



genTagString :: Gen String
genTagString = listOf genTagChar



genTags :: Gen [String]
genTags = do
  n <- choose (0, 10)
  if n > 0
    then vectorOf n (oneOf [genTagString])
    else pure []



genBool :: Gen Bool
genBool = elements [True, False]



genPostDataRaw :: Gen PostData
genPostDataRaw = (PostDataRaw . T.pack) <$> (choose (0, 1024*10) >>= \n -> vectorOf n genAsciiChar)



genPostDataBBCode :: Gen PostData
genPostDataBBCode = (PostDataBBCode . T.pack) <$> (choose (0, 1024*10) >>= \n -> vectorOf n genAsciiChar)



genPostDataEmpty :: Gen PostData
genPostDataEmpty = elements [PostDataEmpty]



genPostData :: Gen PostData
genPostData = oneOf [genPostDataRaw, genPostDataBBCode, genPostDataEmpty]
