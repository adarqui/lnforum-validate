module LN.Generate.Permission (
  genPermission,
  genPermissions,
  emptyPermissions,
  allPermissions
) where



import           LN.T.Permission
import           Test.QuickCheck



genPermission :: Gen Permission
genPermission = elements [Perm_Create, Perm_Read, Perm_Update, Perm_Delete, Perm_Execute]


genPermissions :: Gen [Permission]
genPermissions = listOf genPermission



emptyPermissions :: Permissions
emptyPermissions = []



allPermissions :: Permissions
allPermissions = [Perm_Create, Perm_Read, Perm_Update, Perm_Delete, Perm_Execute]
