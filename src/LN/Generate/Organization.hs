{-# LANGUAGE OverloadedStrings #-}

module LN.Generate.Organization (
  genOrganizationDisplayName,
  genValidOrganizationDisplayName,
  genOrganizationDescription,
  genValidOrganizationDescription,
  buildValidOrganization
) where



import           Data.Either               (isRight)
import           Data.Monoid               ((<>))
import           Data.String.Conversions   (cs)
import           LN.Generate.Internal
import           LN.Sanitize.Organization
import           LN.T.Organization.Request (OrganizationRequest (..))
import           LN.Validate.Organization
import           Test.QuickCheck



genOrganizationDisplayName :: Gen String
genOrganizationDisplayName = genDisplayName'1 0 (maxOrganizationDisplayName*2)

genValidOrganizationDisplayName :: Gen String
genValidOrganizationDisplayName = genDisplayName'1 10 maxOrganizationDisplayName



genOrganizationDescription :: Gen (Maybe String)
genOrganizationDescription = genMaybeDescription (maxOrganizationDescription*2)

genValidOrganizationDescription :: Gen (Maybe String)
genValidOrganizationDescription = genMaybeDescription maxOrganizationDescription



buildValidOrganization :: IO OrganizationRequest
buildValidOrganization = do
  org <- go
  if (isRight $ validateOrganizationRequest org)
    then pure org
    else buildValidOrganization
  where
  go = do
    display_name <- genIO genValidOrganizationDisplayName
    m_desc       <- genIO genValidOrganizationDescription
    membership   <- genIO genMembership
    visibility   <- genIO genVisibility
    let name     =  filter (/= ' ') display_name
    pure $ sanitizeOrganizationRequest $ OrganizationRequest {
      organizationRequestDisplayName = cs display_name,
      organizationRequestDescription = cs <$> m_desc,
      organizationRequestCompany     = "company",
      organizationRequestLocation    = "location",
      organizationRequestEmail       = cs $ name <> "@adarq.org",
      organizationRequestMembership  = membership,
      organizationRequestTags        = [],
      organizationRequestIcon        = Nothing,
      organizationRequestVisibility  = visibility,
      organizationRequestGuard       = 0
    }
