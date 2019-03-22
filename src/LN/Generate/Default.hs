{-# LANGUAGE OverloadedStrings #-}

module LN.Generate.Default (
  defaultApiRequest,
  testApiRequest,
  defaultLikeRequest,
  testLikeRequest,
  defaultUserRequest,
  testUserRequest,
  defaultProfileGender,
  defaultProfileRequest,
  testProfileRequest,
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



defaultUTCTime :: UTCTime
defaultUTCTime = read "2016-01-01 00:00:00.0 UTC" :: UTCTime
