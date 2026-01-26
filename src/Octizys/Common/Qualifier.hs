module Octizys.Common.Qualifier (Qualifier) where

import Data.Text (Text)
import Octizys.Classes.From (From (from))
import Octizys.Common.LogicPath (LogicPath)
import Octizys.Common.Name (Name)
import Prettyprinter (Pretty (pretty))

import Data.Aeson (ToJSON)
import Data.Aeson.Types (ToJSONKey)
import GHC.Generics (Generic, Generically (..))


-- | All process
data Qualifier
  = SingleFile Name
  | ModuleQualifier LogicPath
  | Repl Name
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically Qualifier


instance ToJSONKey Qualifier


instance From Qualifier LogicPath where
  from = ModuleQualifier


instance Pretty Qualifier where
  pretty (SingleFile nm) = pretty nm
  pretty (ModuleQualifier lp) = pretty lp
  pretty (Repl nm) = pretty @Text "!Repl/" <> pretty nm
