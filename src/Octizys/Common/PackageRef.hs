module Octizys.Common.PackageRef (PackageName, makePackageName, PackageRef, calculateHash) where

import Data.Text (Text)
import Prettyprinter (Pretty (pretty))

import Data.Aeson (ToJSON)
import GHC.Generics (Generic, Generically (..))


{- | The name of a package.
As packages maybe (still under development) have non valid
identifier characters, we must have a separate type for them.
-}
newtype PackageName = PackageName'
  { packageNameRaw :: Text
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically PackageName


-- TODO:FIXME
makePackageName :: Text -> PackageName
makePackageName = undefined


instance Pretty PackageName where
  pretty p = pretty p.packageNameRaw


-- TODO:STUB

-- | A newtype around the Version type.
data PackageVersion = PackageVersion'
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically PackageVersion


-- TODO:STUB

{- | A new type over the source of a package, normally this is a url or
an absolute path to a file.
-}
data PackageSource = PackageSource'
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically PackageSource


-- | Information about a package that we can use to identify it.
data PackageIdentity = PackageIdentity'
  { name :: PackageName
  , version :: PackageVersion
  , source :: PackageSource
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically PackageIdentity


{- | Same purpose as the PackageIdentity but this one corresponds to the
current package being compiled. As user can optionally provide those fields,
instead of adding default values we pefered a value that can be null to
represent it.
-}
data OptionalPackageIdentity = OptionalPackageIdentity'
  { name :: Maybe PackageName
  , version :: Maybe PackageVersion
  , source :: Maybe PackageSource
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically OptionalPackageIdentity


-- | Used to disambiguate between multiple versions of the same package
data PackageRef
  = -- | The current package being compiled.
    TargetPackage OptionalPackageIdentity
  | -- | A dependency of our current package.
    NamedPackage PackageIdentity
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically PackageRef


-- TODO:STUB
-- This is going to be replaced with a dependency eventually
data Hash = Hash'
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically Hash


-- TODO:STUB
calculateIdentityHash
  :: Maybe PackageName
  -> Maybe PackageVersion
  -> Maybe PackageSource
  -> Maybe Hash
calculateIdentityHash _ _ _ = Nothing


{- | A stable function to get the hash of the information of a package.
This only returns Nothing if we are at the current package and
the user didn't provide any information about it.
-}
calculateHash :: PackageRef -> Maybe Hash
calculateHash (TargetPackage op) =
  calculateIdentityHash
    op.name
    op.version
    op.source
calculateHash (NamedPackage np) =
  calculateIdentityHash
    (pure np.name)
    (pure np.version)
    (pure np.source)
