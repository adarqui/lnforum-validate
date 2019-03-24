{-# LANGUAGE OverloadedStrings #-}

module LN.Generate.Default (
  defaultApiRequest,
  testApiRequest,
  defaultBoardRequest,
  testBoardRequest,
  defaultLikeRequest,
  testLikeRequest,
  defaultUserRequest,
  testUserRequest,
  defaultProfileGender,
  defaultProfileRequest,
  testProfileRequest,
  defaultThreadRequest,
  testThreadRequest,
  defaultThreadPostRequest,
  testThreadPostRequest,
  defaultUTCTime
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



defaultBoardRequest :: BoardRequest
defaultBoardRequest = BoardRequest {
  boardRequestDisplayName        = "",
  boardRequestDescription        = Nothing,
  boardRequestBoardType          = FixMe,
  boardRequestActive             = True,
  boardRequestIsAnonymous        = False,
  boardRequestCanCreateBoards    = True,
  boardRequestCanCreateThreads   = True,
  boardRequestVisibility         = Public,
  boardRequestIcon               = Nothing,
  boardRequestTags               = [],
  boardRequestGuard              = 0
}

testBoardRequest :: BoardRequest
testBoardRequest = testBoardRequest {
  boardRequestDisplayName = "test",
  boardRequestDescription = Just "test"
}



defaultLikeRequest :: LikeRequest
defaultLikeRequest = LikeRequest {
  likeRequestOpt    = Like,
  likeRequestReason = Nothing,
  likeRequestGuard  = 0
}


testLikeRequest :: LikeRequest
testLikeRequest = defaultLikeRequest { likeRequestReason = Just "test" }



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



defaultThreadRequest :: ThreadRequest
defaultThreadRequest = ThreadRequest {
  threadRequestDisplayName = "",
  threadRequestDescription = Nothing,
  threadRequestSticky      = False,
  threadRequestLocked      = False,
  threadRequestPoll        = Nothing,
  threadRequestIcon        = Nothing,
  threadRequestTags        = [],
  threadRequestGuard       = 0,
  -- state
  threadRequestStateTag    = Nothing
}

testThreadRequest :: ThreadRequest
testThreadRequest = defaultThreadRequest {
  threadRequestDisplayName = "test",
  threadRequestDescription = Just "test"
}



defaultThreadPostRequest :: ThreadPostRequest
defaultThreadPostRequest = ThreadPostRequest {
  threadPostRequestTitle           = Nothing,
  threadPostRequestBody            = PostDataEmpty,
  threadPostRequestTags            = [],
  threadPostRequestPrivateTags     = [],
  threadPostRequestGuard           = 0,
  -- state
  threadPostRequestStateTag        = Nothing,
  threadPostRequestStatePrivateTag = Nothing
}

testThreadPostRequest :: ThreadPostRequest
testThreadPostRequest = defaultThreadPostRequest { threadPostRequestTitle = Just "test" }



defaultUTCTime :: UTCTime
defaultUTCTime = read "2016-01-01 00:00:00.0 UTC" :: UTCTime
