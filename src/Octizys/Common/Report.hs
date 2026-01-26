module Octizys.Common.Report where

import Data.Text (Text)
import Octizys.Pretty.FormatContext (FormatContext, nest)
import Octizys.Pretty.Formatter (Formatter (format))
import Prettyprinter (Doc, Pretty (pretty))
import qualified Prettyprinter as Pretty

import Data.Aeson (ToJSON)
import GHC.Generics (Generic, Generically (..))


data ReportKind
  = ReportError
  | ReportWarn
  | ReportInfo
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically ReportKind


instance Formatter ann (FormatContext ann) ReportKind where
  format _ ReportError = pretty @Text "Error"
  format _ ReportWarn = pretty @Text "Warning"
  format _ ReportInfo = pretty @Text "Info"


data LongDescription ann = LongDescription'
  { preDescription :: Maybe Text
  , source :: Maybe (Doc ann)
  , afterDescription :: Maybe Text
  }
  deriving (Show, Generic)


instance Formatter ann (FormatContext ann) (LongDescription ann) where
  format ctx ld =
    pretty ld.preDescription
      <> case ld.source of
        Just content ->
          case ld.preDescription of
            Just _ ->
              nest
                ctx
                (Pretty.line @ann <> content)
            Nothing ->
              nest
                ctx
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


instance Formatter ann (FormatContext ann) (Report ann) where
  format ctx r =
    format ctx r.reportKind
      <> pretty '>'
      <> pretty r.shortDescription
      <> Pretty.nest
        2
        ( case r.descriptions of
            [] -> mempty
            _ ->
              Pretty.line
                <> Pretty.vsep (format ctx <$> r.descriptions)
        )
