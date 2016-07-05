{-# LANGUAGE OverloadedStrings #-}

module LN.Generate.Thread (
  genThreadDisplayName,
  genValidThreadDisplayName,
  genThreadDescription,
  genValidThreadDescription,
  buildValidThread
) where



import           Data.Either             (isRight)
import           Data.String.Conversions (cs)
import qualified Data.Text               as T (pack)
import           LN.Generate.Internal
import           LN.Sanitize.Thread
import           LN.T.Thread.Request     (ThreadRequest (..))
import           LN.Validate.Thread
import           Test.QuickCheck



genThreadDisplayName :: Gen String
genThreadDisplayName = genDisplayName'1 0 (maxThreadDisplayName*2)

genValidThreadDisplayName :: Gen String
genValidThreadDisplayName = genDisplayName'1 10 maxThreadDisplayName



genThreadDescription :: Gen (Maybe String)
genThreadDescription = genMaybeDescription (maxThreadDescription*2)

genValidThreadDescription :: Gen (Maybe String)
genValidThreadDescription = genMaybeDescription maxThreadDescription



buildValidThread :: IO ThreadRequest
buildValidThread = do
  org <- go
  if (isRight $ validateThreadRequest org)
    then pure org
    else buildValidThread
  where
  go = do
    display_name <- genIO genValidThreadDisplayName
    m_desc       <- genIO genValidThreadDescription
    sticky       <- genIO genBool
    locked       <- genIO genBool
    tags         <- genIO genTags
    pure $ sanitizeThreadRequest $ ThreadRequest {
      threadRequestDisplayName = cs display_name,
      threadRequestDescription = cs <$> m_desc,
      threadRequestSticky      = sticky,
      threadRequestLocked      = locked,
      threadRequestPoll        = Nothing,
      threadRequestTags        = map T.pack tags,
      threadRequestIcon        = Nothing,
      threadRequestGuard       = 0
    }
