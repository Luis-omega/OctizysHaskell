{-# LANGUAGE DeriveLift #-}

module Octizys.Logging.Entry
  ( Field
  , makeField
  , field
  , fieldWithPretty
  , getName
  , getAccurateRepresentation
  , getHumanRepresentation
  , Entry
  , makeEntry
  , getLevel
  , getMessage
  , getFields
  ) where

import Control.Arrow ((<<<))
import Data.Text (Text)
import qualified Data.Text as Text
import Language.Haskell.TH.Syntax (Lift)
import Octizys.Logging.Levels (Level)
import Prettyprinter
  ( Doc
  , Pretty (pretty)
  , defaultLayoutOptions
  , layoutPretty
  )
import Prettyprinter.Render.Text (renderStrict)


data Field = Field'
  { name :: Text
  , accurateRepresentation :: Text
  , humanReadableRepresentation :: Maybe Text
  }
  deriving (Eq, Ord, Show, Lift)


inQuotes :: Text -> Doc ann
inQuotes txt =
  pretty '"'
    <> pretty txt
    <> pretty '"'


prettyJSONField :: Text -> Text -> Doc ann
prettyJSONField name value =
  inQuotes name
    <> pretty ':'
    <> inQuotes value


instance Pretty Field where
  pretty (Field' name rep hrep) =
    pretty '{'
      <> prettyJSONField "name" name
      <> pretty ','
      <> prettyJSONField "representation" rep
      <> pretty ','
      <> maybe mempty (prettyJSONField "humanRepresentation") hrep
      <> pretty '}'


makeField :: Text -> Text -> Maybe Text -> Field
makeField = Field'


render :: Doc ann -> Text
render =
  renderStrict
    <<< layoutPretty defaultLayoutOptions


-- | Create field with provided prettifier function
fieldWithPretty :: Show a => (a -> Doc ann) -> Text -> a -> Field
fieldWithPretty toDoc name value =
  makeField name (Text.pack $ show value) (Just $ render (toDoc value))


-- | Create fields easily if you have a show instance
field
  :: forall a
   . (Pretty a, Show a)
  => Text
  -> a
  -- ^ field name
  -> Field
-- \| value of the field -> Field
field = fieldWithPretty (pretty @a)


getName :: Field -> Text
getName = name


getAccurateRepresentation :: Field -> Text
getAccurateRepresentation = accurateRepresentation


getHumanRepresentation :: Field -> Maybe Text
getHumanRepresentation = humanReadableRepresentation


data Entry = Entry'
  { level :: Level
  , message :: Text
  , fields :: [Field]
  }
  deriving (Eq, Ord, Show, Lift)


makeEntry :: Level -> Text -> [Field] -> Entry
makeEntry = Entry'


getLevel :: Entry -> Level
getLevel = level


getMessage :: Entry -> Text
getMessage = message


getFields :: Entry -> [Field]
getFields = fields
