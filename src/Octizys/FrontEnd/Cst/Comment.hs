-- | Description : This module defines the type for a `Comment`
module Octizys.FrontEnd.Cst.Comment where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import EffectfulParserCombinators.Span (Span)
import GHC.Generics (Generic, Generically (..))


newtype LineComment = LineComment'
  { content :: Text
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically LineComment


newtype BlockComment = BlockComment'
  { content :: [LineComment]
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically BlockComment


data Comment
  = Line {lineComment :: LineComment, span :: Span}
  | Block {blockComment :: BlockComment, span :: Span}
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically Comment
