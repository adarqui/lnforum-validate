{-# LANGUAGE OverloadedStrings #-}

module LN.Generate.Default (
  defaultApiRequest,
  testApiRequest,
  defaultBucketRequest,
  testBucketRequest,
  defaultLeuronRequest,
  dle,
  defaultLeuronTrainingRequest,
  defaultLikeRequest,
  testLikeRequest,
  defaultResourceRequest,
  testResourceRequest,
  defaultStarRequest,
  testStarRequest,
  defaultUserRequest,
  testUserRequest,
  defaultProfileGender,
  defaultProfileRequest,
  testProfileRequest,
  defaultUTCTime,
  defaultTrainingNode,
  defaultBucketRoundRequest
) where



import           Data.Time (UTCTime)
import           Prelude

import           LN.T



defaultApiRequest :: ApiRequest
defaultApiRequest = ApiRequest {
    apiRequestComment = Nothing,
    apiRequestGuard   = 0
}

testApiRequest :: ApiRequest
testApiRequest = defaultApiRequest {
    apiRequestComment = Just "test"
}



defaultBucketRequest :: BucketRequest
defaultBucketRequest = BucketRequest {
  bucketRequestDisplayName  = "",
  bucketRequestDescription  = Nothing,
  bucketRequestScoreLo      = -1000,
  bucketRequestScoreHi      = 3,
  bucketRequestLeurons      = [],
  bucketRequestResources    = [],
  bucketRequestCategories   = [],
  bucketRequestFilters      = [],
  bucketRequestGuard        = 0
}

testBucketRequest :: BucketRequest
testBucketRequest = defaultBucketRequest {
  bucketRequestDisplayName  = "test",
  bucketRequestDescription  = Just "test"
}



defaultLeuronRequest :: LeuronRequest
defaultLeuronRequest = LeuronRequest {
  leuronRequestData          = LnEmpty,
  leuronRequestTitle         = Nothing,
  leuronRequestDescription   = Nothing,
  leuronRequestSection       = Nothing,
  leuronRequestPage          = Nothing,
  leuronRequestExamples      = Nothing,
  leuronRequestStrengths     = Nothing,
  leuronRequestCategories    = [],
  leuronRequestSplits        = Nothing,
  leuronRequestSubstitutions = Nothing,
  leuronRequestTags          = [],
  leuronRequestStyle         = Nothing,
  leuronRequestGuard         = 0
}

dle :: LeuronRequest
dle = defaultLeuronRequest



defaultLeuronTrainingRequest :: LeuronTrainingRequest
defaultLeuronTrainingRequest = LeuronTrainingRequest LTS_View 0



defaultLikeRequest :: LikeRequest
defaultLikeRequest = LikeRequest {
  likeRequestOpt    = Like,
  likeRequestReason = Nothing,
  likeRequestGuard  = 0
}


testLikeRequest :: LikeRequest
testLikeRequest = defaultLikeRequest { likeRequestReason = Just "test" }



defaultResourceRequest :: ResourceRequest
defaultResourceRequest = ResourceRequest {
  resourceRequestDisplayName   = "",
  resourceRequestDescription   = "",
  resourceRequestSource        = SourceNone,
  resourceRequestAuthor        = Nothing,
  resourceRequestPrerequisites = [],
  resourceRequestCategories    = [],
  resourceRequestVisibility    = Public,
  resourceRequestCounter       = 0,
  resourceRequestVersion       = Nothing,
  resourceRequestUrls          = Nothing,
  resourceRequestIcon          = Nothing,
  resourceRequestTags          = [],
  resourceRequestGuard         = 0
}

testResourceRequest :: ResourceRequest
testResourceRequest = defaultResourceRequest {
  resourceRequestDisplayName   = "test",
  resourceRequestDescription   = "test",
  resourceRequestSource        = URL "https://www.adarq.org",
  resourceRequestAuthor        = Just ["test"]
}



defaultStarRequest :: StarRequest
defaultStarRequest = StarRequest {
  starRequestReason = Nothing,
  starRequestGuard  = 0
}

testStarRequest :: StarRequest
testStarRequest = defaultStarRequest { starRequestReason = Just "test" }



defaultUserRequest :: UserRequest
defaultUserRequest = UserRequest {
  userRequestDisplayName = "",
  userRequestFullName    = "",
  userRequestEmail       = "",
  userRequestPlugin      = "",
  userRequestAcceptTOS   = Nothing
}

testUserRequest :: UserRequest
testUserRequest = defaultUserRequest {
  userRequestDisplayName = "test",
  userRequestFullName    = "test",
  userRequestEmail       = "test@test.com",
  userRequestPlugin      = "test"
}



defaultProfileGender :: ProfileGender
defaultProfileGender = GenderUnknown

defaultProfileRequest :: ProfileRequest
defaultProfileRequest = ProfileRequest {
  profileRequestGender        = defaultProfileGender,
  profileRequestBirthdate     = defaultUTCTime,
  profileRequestWebsite       = Nothing,
  profileRequestWebsites      = [],
  profileRequestLocation      = Nothing,
  profileRequestSignature     = Nothing,
  profileRequestDebug         = False,
  profileRequestGuard         = 0,
  -- state
  profileRequestStateWebsites = Nothing
}

testProfileRequest :: ProfileRequest
testProfileRequest = defaultProfileRequest {
  profileRequestWebsite   = Just "https://www.adarq.org",
  profileRequestLocation  = Just "FL",
  profileRequestSignature = Just "go get it."
}



defaultUTCTime :: UTCTime
defaultUTCTime = read "2016-01-01 00:00:00.0 UTC" :: UTCTime



defaultTrainingNode :: TrainingNode
defaultTrainingNode = TrainingNode {
  numTotal          = 0,
  numKnow           = 0,
  numDontKnow       = 0,
  numDontCare       = 0,
  numProtest        = 0,
  honorKnow         = 0,
  honorKnowAt       = Nothing,
  honorDontKnow     = 0,
  honorDontKnowAt   = Nothing,
  honorDontCare     = 0,
  honorDontCareAt   = Nothing,
  honorProtest      = 0,
  honorProtestAt    = Nothing,
  booleanKnow       = 0,
  booleanKnowAt     = Nothing,
  booleanDontKnow   = 0,
  booleanDontKnowAt = Nothing,
  booleanDontCare   = 0,
  booleanDontCareAt = Nothing,
  booleanProtest    = 0,
  booleanProtestAt  = Nothing,
  matchKnow         = 0,
  matchKnowAt       = Nothing,
  matchDontKnow     = 0,
  matchDontKnowAt   = Nothing,
  matchDontCare     = 0,
  matchDontCareAt   = Nothing,
  matchProtest      = 0,
  matchProtestAt    = Nothing,
  subsKnow          = 0,
  subsKnowAt        = Nothing,
  subsDontKnow      = 0,
  subsDontKnowAt    = Nothing,
  subsDontCare      = 0,
  subsDontCareAt    = Nothing,
  subsProtest       = 0,
  subsProtestAt     = Nothing,
  splitsKnow        = 0,
  splitsKnowAt      = Nothing,
  splitsDontKnow    = 0,
  splitsDontKnowAt  = Nothing,
  splitsDontCare    = 0,
  splitsDontCareAt  = Nothing,
  splitsProtest     = 0,
  splitsProtestAt   = Nothing
}



defaultBucketRoundRequest :: BucketRoundRequest
defaultBucketRoundRequest = BucketRoundRequest {
  bucketRoundRequestTrainingStyles = [TS_Simple],
  bucketRoundRequestThreshold      = 3,
  bucketRoundRequestTimeLimit      = 0,
  bucketRoundRequestGuard          = 0
}
