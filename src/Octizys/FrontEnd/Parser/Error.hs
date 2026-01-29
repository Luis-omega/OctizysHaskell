module Octizys.FrontEnd.Parser.Error where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic, Generically (..))
import Prettyprinter (Pretty (pretty))


data OctizysParseError
  = CantParseName Text
  | EmptyImportList
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically OctizysParseError


instance Pretty OctizysParseError where
  pretty (CantParseName str) =
    pretty @Text
      "A bug, we parsed a identifier but is not a valid identifier: "
      <> pretty str
  pretty EmptyImportList = pretty @Text "All unqualified imports must provide a list of imports."
