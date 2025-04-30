-- | Description : Pretty printer definition for comments.
module Octizys.Pretty.Comment where

import Control.Arrow ((<<<))
import Data.Text (Text)
import Octizys.Cst.Comment
  ( BlockComment (BlockComment', content)
  , Comment (Block, Line)
  , LineComment (LineComment', content)
  )
import Prettyprinter (Doc, Pretty (pretty), vsep)


pText :: Text -> Doc ann
pText = pretty @Text


prettyLine :: LineComment -> Doc ann
prettyLine LineComment' {content = _content} =
  pText "--" <> pText _content


prettyBlock :: BlockComment -> Doc ann
prettyBlock BlockComment' {content = _content} =
  pText "{-"
    <> vsep ((pText <<< \(LineComment' x) -> x) <$> _content)
    <> pText "-}"


prettyComment :: Comment -> Doc ann
prettyComment (Line l _) = prettyLine l
prettyComment (Block b _) = prettyBlock b
