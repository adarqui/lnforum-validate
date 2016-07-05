{-# LANGUAGE OverloadedStrings #-}

module LN.Generate.Board (
  genBoardDisplayName,
  genValidBoardDisplayName,
  genBoardDescription,
  genValidBoardDescription,
  buildValidBoard
) where



import           Control.Monad.IO.Class  (liftIO)
import           Data.Either             (isRight)
import           Data.String.Conversions (cs)
import qualified Data.Text               as T (pack)
import           LN.Generate.Internal
import           LN.Sanitize.Board
import           LN.T.Board.Request      (BoardRequest (..))
import           LN.Validate.Board
import           Test.QuickCheck



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
    display_name          <- liftIO $ generate genValidBoardDisplayName
    m_desc                <- liftIO $ generate genValidBoardDescription
    is_anonymous          <- liftIO $ generate genBool
    can_create_sub_boards <- liftIO $ generate genBool
    can_create_threads    <- liftIO $ generate genBool
    suggested_tags        <- liftIO $ generate genTags
    tags                  <- liftIO $ generate genTags
    pure $ sanitizeBoardRequest $ BoardRequest {
      boardRequestDisplayName        = cs display_name,
      boardRequestDescription        = cs <$> m_desc,
      boardRequestIsAnonymous        = is_anonymous,
      boardRequestCanCreateSubBoards = can_create_sub_boards,
      boardRequestCanCreateThreads   = can_create_threads,
      boardRequestSuggestedTags      = map T.pack suggested_tags,
      boardRequestTags               = map T.pack tags,
      boardRequestIcon               = Nothing,
      boardRequestGuard              = 0
    }
