-- | Description : Pretty printer definition for comments.
module Octizys.FrontEnd.Format.Comment where

import Control.Arrow ((<<<))
import Octizys.Common.Format.Config (formatText)
import Octizys.FrontEnd.Cst.Comment
  ( BlockComment (BlockComment', content)
  , Comment (Block, Line)
  , LineComment (LineComment', content)
  )
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
