module Octizys.Cst.SourceInfo where

import Octizys.Common.LogicPath (LogicPath)
import Octizys.Common.Name (Name)
import Octizys.Cst.Comment (Comment)
import Octizys.Cst.Span (Span)


data SourceInfo = SourceInfo'
  { span :: Span
  , preComments :: [Comment]
  , afterComment :: Maybe Comment
  }
  deriving (Show, Eq, Ord)


data SourceVariable = SourceVariable'
  { qualifier :: Maybe LogicPath
  , name :: Name
  }
  deriving (Show, Eq, Ord)
