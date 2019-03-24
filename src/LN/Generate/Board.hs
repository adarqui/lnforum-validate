{-# LANGUAGE OverloadedStrings #-}

module LN.Generate.Board (
  genBoardDisplayName,
  genValidBoardDisplayName,
  genBoardDescription,
  genValidBoardDescription,
  buildValidBoard
) where



import           Data.Either             (isRight)
import           Data.String.Conversions (cs)
import qualified Data.Text               as T (pack)
import           Prelude
import           Test.QuickCheck

import           LN.Generate.Default
import           LN.Generate.Internal
import           LN.Sanitize.Board
import           LN.T.Board              (BoardType(..), BoardRequest (..))
import           LN.Validate.Board



genBoardDisplayName :: Gen String
genBoardDisplayName = genDisplayName'1 0 (maxBoardDisplayName*2)

genValidBoardDisplayName :: Gen String
genValidBoardDisplayName = genDisplayName'1 10 maxBoardDisplayName



genBoardDescription :: Gen (Maybe String)
genBoardDescription = genMaybeDescription (maxBoardDescription*2)

genValidBoardDescription :: Gen (Maybe String)
genValidBoardDescription = genMaybeDescription maxBoardDescription



buildValidBoard :: IO BoardRequest
buildValidBoard = do
  org <- go
  if (isRight $ validateBoardRequest org)
    then pure org
    else buildValidBoard
  where
  go = do
    display_name          <- genIO genValidBoardDisplayName
    m_desc                <- genIO genValidBoardDescription
    is_anonymous          <- genIO genBool
    can_create_sub_boards <- genIO genBool
    can_create_threads    <- genIO genBool
    tags                  <- genIO genTags
    pure $ sanitizeBoardRequest $ defaultBoardRequest {
      boardRequestDisplayName        = cs display_name,
      boardRequestDescription        = cs <$> m_desc,
      boardRequestBoardType          = FixMe,
      boardRequestIsAnonymous        = is_anonymous,
      boardRequestCanCreateBoards    = can_create_sub_boards,
      boardRequestCanCreateThreads   = can_create_threads,
      boardRequestTags               = map T.pack tags,
      boardRequestIcon               = Nothing,
      boardRequestGuard              = 0
    }
