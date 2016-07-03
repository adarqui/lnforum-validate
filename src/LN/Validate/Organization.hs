{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LN.Validate.Organization (
  validateOrganizationRequest
) where



import           Control.Monad             (void)
import           LN.Sanitize.Organization  (sanitizeOrganizationRequest)
import           LN.T.Error                (ValidationError)
import           LN.T.Organization.Request (OrganizationRequest (..))
import           LN.Validate.Internal



validateOrganizationRequest :: OrganizationRequest -> Either ValidationError OrganizationRequest
validateOrganizationRequest org_req = do
  void $ isValid (Just "display_name") $ isValidDisplayName organizationRequestDisplayName
  void $ isValid (Just "email")        $ isValidEmail organizationRequestEmail
  void $ isValid (Just "company")      $ isValidNonEmptyString organizationRequestCompany
  void $ isValid (Just "location")     $ isValidNonEmptyString organizationRequestLocation
  Right z
  where
  z@OrganizationRequest{..} = sanitizeOrganizationRequest org_req
