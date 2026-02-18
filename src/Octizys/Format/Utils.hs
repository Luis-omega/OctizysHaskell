module Octizys.Format.Utils where

import Control.Arrow ((<<<))
import Data.Text (Text)

import Effectful (Eff)
import Effectful.Error.Static (Error, throwError)
import Effectful.Internal.Effect ((:>))

import Prettyprinter
  ( Doc
  , Pretty (pretty)
  , defaultLayoutOptions
  , layoutPretty
  )
import qualified Prettyprinter as Pretty
import Prettyprinter.Render.Text (renderStrict)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Octizys.Format.Class (Formattable (format))
import Octizys.Format.Config (Configuration, getIndentation)


-- | The separator between a function arguments and it's output.
functionSeparator :: Doc ann
functionSeparator = text "|-"


-- | The separator between a function type and it's body.
functionBodySeparator :: Doc ann
functionBodySeparator = text "|-"


-- | It marks the start of a function
functionStart :: Doc ann
functionStart = text "\\"


-- | The pretty function specialized for text.
text :: Text -> Doc ann
text = pretty


-- | Nest the document in the given configuration amount.
nest :: Configuration -> Doc ann -> Doc ann
nest configuration = Pretty.nest (getIndentation configuration)


-- | Set the nest level and adds a line before the item
indentPretty :: Pretty a => Configuration -> a -> Doc ann
indentPretty configuration x = indentDoc configuration (pretty x)


-- | Set the nest level and adds a line before the item
indentFormat :: Formattable a => Configuration -> a -> Doc ann
indentFormat configuration x = indentDoc configuration (format configuration x)


-- | Set the nest level and adds a line before the document
indentDoc :: Configuration -> Doc ann -> Doc ann
indentDoc configuration x = nest configuration (Pretty.line <> x)


-- | Add the given message before the item, the item is separated from it with a line and is indented.
prettyWithHeader :: Pretty a => Configuration -> Text -> a -> Doc ann
prettyWithHeader configuration header value =
  pretty header
    <> indentPretty configuration value


-- | Add the given message before the item, the item is separated from it with a line and is indented.
formatWithHeader :: Formattable a => Configuration -> Text -> a -> Doc ann
formatWithHeader configuration header value =
  pretty header
    <> indentFormat configuration value


-- | Prettify the input with default options for rendering.
renderPretty :: Pretty a => a -> Text
renderPretty x =
  renderDoc (pretty x)


-- | Format the input with default options for rendering.
render :: Formattable a => Configuration -> a -> Text
render configuration x =
  renderDoc (format configuration x)


-- | Render a document with default values
renderDoc :: Doc ann -> Text
renderDoc doc =
  renderStrict $ layoutPretty defaultLayoutOptions doc


{- | Format tuples of two items.
It uses parens to enclose the items.
-}
formatTupleItemsWith
  :: Configuration
  -> (Configuration -> a -> Doc ann)
  -- ^ First item formatter
  -> (Configuration -> b -> Doc ann)
  -- ^ Second item formatter
  -> Doc ann
  -- ^ Item separator
  -> (a, b)
  -- ^ Tuple to be formatted
  -> Doc ann
formatTupleItemsWith c f g sep (x, y) =
  Pretty.parens (f c x <> Pretty.line <> sep <> g c y)


{- | Format tuples of two items.
It uses parens to enclose the items.
-}
formatTupleItems
  :: Formattable a
  => Formattable b
  => Configuration
  -> Doc ann
  -- ^ Item separator
  -> (a, b)
  -- ^ Tuple to be formatted
  -> Doc ann
formatTupleItems c =
  formatTupleItemsWith c format format


{- | Formats the items of a list using the given separator
to separate the items. It doesn't surrounds the sequence with anything.
-}
formatListItemsWith
  :: forall a t ann
   . Foldable t
  => Functor t
  => Configuration
  -> (Configuration -> a -> Doc ann)
  -- ^ Item formatter
  -> Doc ann
  -- ^ Item separator
  -> t a
  -> Doc ann
formatListItemsWith configuration toDoc sep items =
  Pretty.concatWith
    ( \x y ->
        x <> Pretty.line <> sep <> y
    )
    (toDoc configuration <$> items)


{- | Formats the items of a list using the given separator
to separate the items. It doesn't surrounds the sequence with anything.
-}
formatListItems
  :: forall a t ann
   . Foldable t
  => Functor t
  => Formattable a
  => Configuration
  -> Doc ann
  -- ^ Item separator
  -> t a
  -> Doc ann
formatListItems configuration = formatListItemsWith configuration format


formatListWithSep
  :: Configuration
  -> (Configuration -> a -> Doc ann)
  -> Doc ann
  -> Doc ann
  -> [a]
  -> Doc ann
  -> Doc ann
formatListWithSep _ _ _ l [] r = l <> r
formatListWithSep configuration f sep l ls r =
  l
    <> Pretty.group
      ( nest
          configuration
          ( Pretty.line
              <> formatListItemsWith configuration f sep ls
          )
      )
    <> Pretty.line
    <> r


formatListWith
  :: Configuration
  -> (Configuration -> a -> Doc ann)
  -> [a]
  -> Doc ann
formatListWith configuration f ls =
  formatListWithSep
    configuration
    f
    Pretty.comma
    Pretty.lbracket
    ls
    Pretty.rbracket


formatList :: Formattable a => Configuration -> [a] -> Doc ann
formatList configuration = formatListWith configuration format


formatMapWith
  :: Configuration
  -> (Configuration -> (k, v) -> Doc ann)
  -> Doc ann
  -> Map k v
  -> Doc ann
formatMapWith configuration f sep m =
  let
    asList = Map.toList m
   in
    formatListWithSep
      configuration
      f
      sep
      Pretty.lbracket
      asList
      Pretty.rbracket


formatMap
  :: Formattable k
  => Formattable v
  => Configuration
  -> Map k v
  -> Doc ann
formatMap configuration =
  formatMapWith
    configuration
    ( \c (k, v) ->
        (Pretty.group <<< nest c)
          (formatTupleItemsWith c format format Pretty.comma (k, v))
    )
    Pretty.comma


formatSetWith
  :: Configuration
  -> (Configuration -> a -> Doc ann)
  -> Doc ann
  -> Set a
  -> Doc ann
formatSetWith configuration f sep s =
  formatListWithSep
    configuration
    f
    sep
    Pretty.lbrace
    (Set.toList s)
    Pretty.rbrace


formatSet
  :: Formattable a
  => Configuration
  -> Set a
  -> Doc ann
formatSet configuration = formatSetWith configuration format Pretty.comma


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
throwPrettyError = throwError <<< renderPretty


throwFormatError
  :: Error Text :> es
  => Formattable a
  => Configuration
  -> a
  -> Eff es a
throwFormatError configuration = throwError <<< render configuration
