module Octizys.Common.Report where

import Data.Text (Text)
import Prettyprinter (Doc, Pretty (pretty))
import qualified Prettyprinter as Pretty

import Data.Aeson (ToJSON)
import GHC.Generics (Generic, Generically (..))
import qualified Octizys.Format.Config as Format
import qualified Octizys.Format.Utils as Format


data ReportKind
  = ReportError
  | ReportWarn
  | ReportInfo
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically ReportKind


instance Pretty ReportKind where
  pretty ReportError = pretty @Text "Error"
  pretty ReportWarn = pretty @Text "Warning"
  pretty ReportInfo = pretty @Text "Info"


data LongDescription ann = LongDescription'
  { preDescription :: Maybe Text
  , source :: Maybe (Doc ann)
  , afterDescription :: Maybe Text
  }
  deriving (Show, Generic)


formatLongDescription
  :: Format.Configuration -> LongDescription ann -> Doc ann
formatLongDescription configuration ld =
  pretty ld.preDescription
    <> case ld.source of
      Just content ->
        case ld.preDescription of
          Just _ ->
            Format.nest
              configuration
              (Pretty.line <> content)
          Nothing ->
            Format.nest
              configuration
              content
      Nothing -> mempty
    <> case ld.afterDescription of
      Just after ->
        -- TODO: FIXME: the line shouldn't be here if both of the
        -- previous elements are missing
        Pretty.line <> pretty after
      Nothing -> mempty


data Report ann = Report'
  { reportKind :: ReportKind
  , shortDescription :: Text
  , descriptions :: [LongDescription ann]
  }
  deriving (Show, Generic)


formatReport :: Format.Configuration -> Report ann -> Doc ann
formatReport configuration r =
  pretty r.reportKind
    <> pretty '>'
    <> pretty r.shortDescription
    <> Format.nest
      configuration
      ( case r.descriptions of
          [] -> mempty
          _ ->
            Pretty.line
              <> Pretty.vsep
                ( formatLongDescription configuration
                    <$> r.descriptions
                )
      )
