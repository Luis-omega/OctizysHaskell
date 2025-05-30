module Octizys.Common.PackageName (PackageName, makePackageName) where

import Data.Text (Text)
import Prettyprinter (Pretty (pretty))


{- | The name of a package.
As packages maybe (still under development) have non valid
identifier characters, we must have a separate type for them.
-}
newtype PackageName = PackageName'
  { packageNameRaw :: Text
  }
  deriving (Show, Eq, Ord)


makePackageName :: Text -> PackageName
makePackageName = undefined


instance Pretty PackageName where
  pretty p = pretty p.packageNameRaw

