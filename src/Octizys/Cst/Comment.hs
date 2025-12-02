-- | Description : This module defines the type for a `Comment`
module Octizys.Cst.Comment where

import Data.Text (Text)
import EffectfulParserCombinators.Span (Span)


newtype LineComment = LineComment'
  { content :: Text
  }
  deriving (Show, Eq, Ord)


newtype BlockComment = BlockComment'
  { content :: [LineComment]
  }
  deriving (Show, Eq, Ord)


data Comment
  = Line {lineComment :: LineComment, span :: Span}
  | Block {blockComment :: BlockComment, span :: Span}
  deriving (Show, Eq, Ord)
