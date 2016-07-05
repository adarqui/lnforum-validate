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
  genMembership,
  genVisibility,
  genProfileGender,
  genTags,
  genBool
) where




import           Control.Monad.IO.Class (liftIO)
import           LN.T.Membership        (Membership (..))
import           LN.T.Profile           (ProfileGender (..))
import           LN.T.Visibility        (Visibility (..))
import           Test.QuickCheck



genIO :: forall a. Gen a -> IO a
genIO gen = liftIO $ generate gen



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



genMembership :: Gen Membership
genMembership = elements [Membership_InviteOnly, Membership_RequestInvite, Membership_Join, Membership_Locked]



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
