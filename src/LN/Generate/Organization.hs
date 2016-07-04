{-# LANGUAGE OverloadedStrings #-}

module LN.Generate.Organization (
  genOrganizationDisplayName,
  genValidOrganizationDisplayName,
  genOrganizationDescription,
  genValidOrganizationDescription,
  buildValidOrganization
) where



import           Control.Monad.IO.Class    (liftIO)
import           Data.Monoid               ((<>))
import           Data.String.Conversions   (cs)
import           LN.Generate.Internal
import           LN.T.Membership           (Membership (..))
import           LN.T.Organization.Request (OrganizationRequest (..))
import           LN.T.Visibility           (Visibility (..))
import           LN.Validate.Organization
import           Test.QuickCheck



genOrganizationDisplayName :: Gen String
genOrganizationDisplayName = genDisplayName'1 0 (maxOrganizationDisplayName*2)

genValidOrganizationDisplayName :: Gen String
genValidOrganizationDisplayName = genDisplayName'1 minOrganizationDisplayName maxOrganizationDisplayName



genOrganizationDescription :: Gen (Maybe String)
genOrganizationDescription = genMaybeDescription (maxOrganizationDescription*2)

genValidOrganizationDescription :: Gen (Maybe String)
genValidOrganizationDescription = genMaybeDescription maxOrganizationDescription



buildValidOrganization :: IO OrganizationRequest
buildValidOrganization = do
  display_name <- liftIO $ generate genValidOrganizationDisplayName
  m_desc       <- liftIO $ generate genValidOrganizationDescription
  let name     =  filter (/= ' ') display_name
  pure $ OrganizationRequest {
    organizationRequestDisplayName = cs display_name,
    organizationRequestDescription = cs <$> m_desc,
    organizationRequestCompany     = "company",
    organizationRequestLocation    = "location",
    organizationRequestEmail       = cs $ name <> "@adarq.org",
    organizationRequestMembership  = Membership_Join,
    organizationRequestTags        = [],
    organizationRequestIcon        = Nothing,
    organizationRequestVisibility  = Public,
    organizationRequestGuard       = 0
  }
