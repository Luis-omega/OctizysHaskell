{- | Description : Pretty printer definition for comments.
-}
module Octizys.Pretty.Comment where
import Octizys.Cst.Comment (LineComment (LineComment', content), BlockComment(BlockComment',content), Comment (Line, Block))
import Prettyprinter (Doc, Pretty (pretty), vsep)
import Data.Text(Text)
import Control.Arrow ((<<<))

pText :: Text -> Doc ann
pText = pretty @Text


prettyLine :: LineComment -> Doc ann
prettyLine LineComment' {content=_content} = 
  pText "--" <> pText _content

prettyBlock :: BlockComment -> Doc ann
prettyBlock BlockComment' {content=_content} = 
  vsep ((pText <<< \ (LineComment' x) -> x) <$> _content)

prettyComment :: Comment -> Doc ann
prettyComment (Line l _) = prettyLine l
prettyComment (Block b _) = prettyBlock b
