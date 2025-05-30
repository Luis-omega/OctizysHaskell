module Octizys.Common.Qualifier (Qualifier) where

import Octizys.Common.LogicPath (LogicPath)
import Octizys.Common.Name (Name)
import Octizys.Common.PackageName (PackageName)
import Prettyprinter (Pretty (pretty))
import Data.Text (Text)


-- | All process
data Qualifier
  = SingleFile Name
  | ModuleQualifier PackageName LogicPath
  | Repl Name
  deriving (Show, Eq, Ord)


instance Pretty Qualifier where
  pretty (SingleFile nm) = pretty nm
  pretty (ModuleQualifier pkg lp) =
    pretty pkg
      <> pretty '/'
      <> pretty lp
  pretty (Repl nm) = pretty @Text "!Repl/" <> pretty nm

