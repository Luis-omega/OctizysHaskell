module Octizys.FrontEnd.Cst.SourceInfo
  ( SourceInfo (span, preComments, afterComment)
  , makeSourceInfo
  , SourceVariable
  ) where

import Data.Aeson (ToJSON)
import EffectfulParserCombinators.Span (Span)
import GHC.Generics (Generic, Generically (..))
import Octizys.Classes.From (From (from))
import Octizys.Common.Id (SymbolOriginInfo)
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


newtype SourceVariable = SourceVariable' SymbolOriginInfo
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically SourceVariable


instance From SourceVariable ([Name], Name) where
  from x = SourceVariable' $ from x


instance From SymbolOriginInfo SourceVariable where
  from (SourceVariable' x) = x


instance From SourceVariable SymbolOriginInfo where
  from = SourceVariable'


instance Pretty SourceVariable where
  pretty (SourceVariable' sv) = pretty sv
