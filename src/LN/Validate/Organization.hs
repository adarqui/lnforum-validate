{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LN.Validate.Organization (
  validateOrganizationRequest,
  isValidOrganizationDisplayName,
  minOrganizationDisplayName,
  maxOrganizationDisplayName,
  maxOrganizationDescription
) where



import           Control.Monad            (void)
import           Data.Ifte                (teifEither)
import           Data.Text                (Text)
import           LN.Sanitize.Organization (sanitizeOrganizationRequest)
import           LN.T.Error               (ValidationError (..),
                                           ValidationErrorCode (..))
import           LN.T.Organization        (OrganizationRequest (..))
import           LN.Validate.Internal



validateOrganizationRequest :: OrganizationRequest -> Either ValidationError OrganizationRequest
validateOrganizationRequest org_req = do
  void $ isValid (Just "display_name") $ isValidOrganizationDisplayName organizationRequestDisplayName
  void $ isValid (Just "email")        $ isValidEmail organizationRequestEmail
  void $ isValid (Just "company")      $ isValidNonEmptyString organizationRequestCompany
  void $ isValid (Just "location")     $ isValidNonEmptyString organizationRequestLocation
  Right z
  where
  z@OrganizationRequest{..} = sanitizeOrganizationRequest org_req



isValidOrganizationDisplayName :: Text -> Either ValidationErrorCode Text
isValidOrganizationDisplayName name = do
  void $ isValidNonEmptyString name
  void $ isValidLength minOrganizationDisplayName maxOrganizationDisplayName name
  teifEither name Validate_InvalidCharacters $ onlyDisplayNameChars name



minOrganizationDisplayName :: Int
minOrganizationDisplayName = 1

maxOrganizationDisplayName :: Int
maxOrganizationDisplayName = 32

maxOrganizationDescription :: Int
maxOrganizationDescription = 132
