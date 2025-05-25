-- | Description : Pretty printer definition for comments.
module Octizys.Pretty.Cst.Comment where

import Control.Arrow ((<<<))
import Octizys.Cst.Comment
  ( BlockComment (BlockComment', content)
  , Comment (Block, Line)
  , LineComment (LineComment', content)
  )
import Octizys.Pretty.FormatContext (formatText)
import Prettyprinter (Doc, vsep)


formatLine :: LineComment -> Doc ann
formatLine LineComment' {content = _content} =
  formatText "--" <> formatText _content


formatBlock :: BlockComment -> Doc ann
formatBlock BlockComment' {content = _content} =
  formatText "{-"
    <> vsep ((formatText <<< \(LineComment' x) -> x) <$> _content)
    <> formatText "-}"


formatComment :: Comment -> Doc ann
formatComment (Line l _) = formatLine l
formatComment (Block b _) = formatBlock b
