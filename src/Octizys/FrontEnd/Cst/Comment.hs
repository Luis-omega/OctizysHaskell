-- | Description : This module defines the type for a `Comment`
module Octizys.FrontEnd.Cst.Comment where

import Control.Arrow ((<<<))
import Data.Aeson (ToJSON)
import Data.Text (Text)
import EffectfulParserCombinators.Span (Span)
import GHC.Generics (Generic, Generically (..))
import Octizys.Format.Class (Formattable (format))
import qualified Octizys.Format.Utils as Format
import Prettyprinter (Doc)
import qualified Prettyprinter as Pretty


newtype LineComment = LineComment'
  { content :: Text
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically LineComment


instance Formattable LineComment where
  format _ = formatLine


newtype BlockComment = BlockComment'
  { content :: [LineComment]
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically BlockComment


instance Formattable BlockComment where
  format _ = formatBlock


data Comment
  = Line {lineComment :: LineComment, span :: Span}
  | Block {blockComment :: BlockComment, span :: Span}
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically Comment


instance Formattable Comment where
  format _ = formatComment


-- * Format


formatLine :: LineComment -> Doc ann
formatLine LineComment' {content = _content} =
  Format.text "--" <> Format.text _content


formatBlock :: BlockComment -> Doc ann
formatBlock BlockComment' {content = _content} =
  Format.text "{-"
    <> Pretty.vsep ((Format.text <<< \(LineComment' x) -> x) <$> _content)
    <> Format.text "-}"


formatComment :: Comment -> Doc ann
formatComment (Line l _) = formatLine l
formatComment (Block b _) = formatBlock b
