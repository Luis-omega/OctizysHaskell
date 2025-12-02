module Octizys.Cst.SourceInfo where

import qualified Data.List.NonEmpty as NonEmpty
import EffectfulParserCombinators.Span (Span)
import Octizys.Classes.From (From (from))
import Octizys.Common.LogicPath (LogicPath)
import Octizys.Common.Name (Name)
import Octizys.Cst.Comment (Comment)


data SourceInfo = SourceInfo'
  { span :: Span
  , preComments :: [Comment]
  , afterComment :: Maybe Comment
  }
  deriving (Show, Eq, Ord)


makeSourceInfo :: Span -> [Comment] -> Maybe Comment -> SourceInfo
makeSourceInfo = SourceInfo'


data SourceVariable = SourceVariable'
  { qualifier :: Maybe LogicPath
  , name :: Name
  }
  deriving (Show, Eq, Ord)


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
