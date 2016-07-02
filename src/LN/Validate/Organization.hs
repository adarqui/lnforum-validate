{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LN.Validate.Organization (
  validateOrganizationRequest
) where



import           Control.Monad             (void)
import           LN.T.Error
import           LN.T.Organization.Request (OrganizationRequest (..))
import           LN.Validate.Internal



validateOrganizationRequest :: OrganizationRequest -> Either ValidationError OrganizationRequest
validateOrganizationRequest z@OrganizationRequest{..} = do
  void $ invalid (Just "display_name") $ isValidName organizationRequestDisplayName
  void $ invalid (Just "email") $ isValidEmail organizationRequestEmail
  void $ invalid (Just "company") $ isValidNonEmptyString organizationRequestCompany
  void $ invalid (Just "location") $ isValidNonEmptyString organizationRequestLocation
  Right z
