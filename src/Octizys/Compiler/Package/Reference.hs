module Octizys.Compiler.Package.Reference (PackageName, makePackageName, Reference, calculateHash) where

import Data.Text (Text)
import Prettyprinter (Pretty (pretty))

import Data.Aeson (ToJSON)
import GHC.Generics (Generic, Generically (..))
import Octizys.Common.Name (Name, makeName)
import Octizys.Common.Version (Version)


-- | The name of a package.
newtype PackageName = PackageName'
  { packageNameRaw :: Name
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically PackageName


makePackageName :: Text -> Maybe PackageName
makePackageName t = PackageName' <$> makeName t


instance Pretty PackageName where
  pretty p = pretty p.packageNameRaw


-- | A newtype around the Version type.
newtype PackageVersion = PackageVersion'
  { unPackageVersion :: Version
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically PackageVersion


-- | The source of a package can be an url or if locally created a path
newtype PackageSource = PackageSource' Text
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


instance Pretty PackageIdentity where
  pretty p =
    pretty p.name


{- | Same purpose as the PackageIdentity but this one corresponds to the
current package being compiled.
We allow user to omit the name and the version, instead of
forcing a default value, we omit them also.
Note that theres always a source for this package
-}
data OptionalPackageIdentity = OptionalPackageIdentity'
  { name :: Maybe PackageName
  , version :: Maybe PackageVersion
  , source :: PackageSource
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically OptionalPackageIdentity


instance Pretty OptionalPackageIdentity where
  pretty p =
    case p.name of
      Just n -> pretty n
      Nothing -> pretty @Text "!DefaultPackage"


-- | Used to disambiguate between multiple versions of the same package
data Reference
  = -- | The current package being compiled.
    TargetPackage OptionalPackageIdentity
  | -- | A dependency of our current package.
    NamedPackage PackageIdentity
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically Reference


instance Pretty Reference where
  pretty (TargetPackage p) = pretty p
  pretty (NamedPackage p) = pretty p


-- TODO:STUB
-- This is going to be replaced with a dependency eventually
data Hash = Hash'
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically Hash


-- TODO:STUB
calculateIdentityHash
  :: Maybe PackageName
  -> Maybe PackageVersion
  -> PackageSource
  -> Maybe Hash
calculateIdentityHash _ _ _ = Nothing


{- | A stable function to get the hash of the information of a package.
This only returns Nothing if we are at the current package and
the user didn't provide any information about it.
-}
calculateHash :: Reference -> Maybe Hash
calculateHash (TargetPackage op) =
  calculateIdentityHash
    op.name
    op.version
    op.source
calculateHash (NamedPackage np) =
  calculateIdentityHash
    (pure np.name)
    (pure np.version)
    np.source
