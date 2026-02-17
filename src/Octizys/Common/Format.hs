module Octizys.Common.Format where

import Control.Arrow ((<<<))
import Data.Text (Text)
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Prettyprinter
  ( Doc
  , Pretty
  , align
  , concatWith
  , defaultLayoutOptions
  , layoutPretty
  , line
  , nest
  , pretty
  , (<+>)
  )
import Prettyprinter.Render.Text (renderStrict)


defaultIndentationSpaces :: Int
defaultIndentationSpaces = 2


pText :: Text -> Doc ann
pText = pretty


-- | Add 4 to document indentation and put a line before the document.
indentPretty :: Pretty a => a -> Doc ann
indentPretty x = indentDoc (pretty x)


indentDoc :: Doc ann -> Doc ann
indentDoc x = nest defaultIndentationSpaces (line <> x)


prettyWithHeader :: Pretty a => Text -> a -> Doc ann
prettyWithHeader header value =
  pretty header
    <> indentPretty value


render :: Pretty a => a -> Text
render x =
  renderDoc (pretty x)


renderDoc :: Doc ann -> Text
renderDoc doc =
  renderStrict $ layoutPretty defaultLayoutOptions doc


prettyItemList
  :: forall x y ann
   . Pretty x
  => Pretty y
  => [(x, y)]
  -> Doc ann
  -> Doc ann
  -> Doc ann
prettyItemList items sep binder =
  let
    prettyItem :: (x, y) -> Doc ann
    prettyItem (li, ri) =
      pretty li
        <+> binder
        <+> pretty ri
   in
    (align <<< nest defaultIndentationSpaces)
      ( concatWith
          ( \x y ->
              x <> line <> sep <+> y
          )
          (prettyItem <$> items)
      )


throwDocError
  :: Error Text :> es
  => Doc ann
  -> Eff es a
throwDocError = throwError <<< renderDoc


throwPrettyError
  :: Error Text :> es
  => Pretty a
  => a
  -> Eff es a
throwPrettyError = throwError <<< render
