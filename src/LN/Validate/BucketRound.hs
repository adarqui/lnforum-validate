{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LN.Validate.BucketRound (
  validateBucketRoundRequest
) where



import           Control.Monad        (void)

import           LN.T.BucketRound     (BucketRoundRequest (..))
import           LN.T.Error           (ValidationError (..))
import           LN.Validate.Internal



validateBucketRoundRequest :: BucketRoundRequest -> Either ValidationError BucketRoundRequest
validateBucketRoundRequest round_req = do
  void $ isValid (Just "training_styles") $ isValidNonEmptyList bucketRoundRequestTrainingStyles
  Right z
  where
  z@BucketRoundRequest{..} = round_req {- sanitizeBucketRoundRequest round_req -}
