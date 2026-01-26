module Octizys.FrontEnd.Cst.SourceInfo where

import Data.Aeson (ToJSON)
import qualified Data.List.NonEmpty as NonEmpty
import EffectfulParserCombinators.Span (Span)
import GHC.Generics (Generic, Generically (..))
import Octizys.Classes.From (From (from))
import Octizys.Common.LogicPath (LogicPath)
import Octizys.Common.Name (Name)
import Octizys.FrontEnd.Cst.Comment (Comment)
import Prettyprinter (Pretty (pretty))


data SourceInfo = SourceInfo'
  { span :: Span
  , preComments :: [Comment]
  , afterComment :: Maybe Comment
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically SourceInfo


makeSourceInfo :: Span -> [Comment] -> Maybe Comment -> SourceInfo
makeSourceInfo = SourceInfo'


data SourceVariable = SourceVariable'
  { qualifier :: Maybe LogicPath
  , name :: Name
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically SourceVariable


instance From SourceVariable ([Name], Name) where
  from (ps, name) =
    case ps of
      [] -> SourceVariable' {qualifier = Nothing, name}
      (p : remain) ->
        SourceVariable'
          { qualifier =
              Just (from (p NonEmpty.:| remain))
          , name
          }


instance Pretty SourceVariable where
  pretty sv =
    case sv.qualifier of
      Just qual -> pretty qual <> pretty '/' <> pretty sv.name
      Nothing -> pretty sv.name
