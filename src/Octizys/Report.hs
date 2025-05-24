module Octizys.Report where

import Data.Text (Text)
import Prettyprinter (Doc, Pretty (pretty))
import qualified Prettyprinter as Pretty


data ReportKind
  = ReportError
  | ReportWarn
  | ReportInfo
  deriving (Show, Eq, Ord)


instance Pretty ReportKind where
  pretty ReportError = pretty @Text "Error"
  pretty ReportWarn = pretty @Text "Warning"
  pretty ReportInfo = pretty @Text "Info"


data LongDescription ann = LongDescription'
  { preDescription :: Maybe Text
  , source :: Maybe (Doc ann)
  , afterDescription :: Maybe Text
  }


prettyLongDescription
  :: forall ann
   . LongDescription ann
  -> Doc ann
prettyLongDescription ld =
  pretty ld.preDescription
    <> case source ld of
      Just content ->
        Pretty.nest
          2
          ( Pretty.line @ann <> content
          )
      Nothing -> mempty
    <> case ld.afterDescription of
      Just after -> Pretty.line <> pretty after
      Nothing -> mempty


data Report ann = Report'
  { reportKind :: ReportKind
  , shortDescription :: Text
  , descriptions :: [LongDescription ann]
  }


prettyReport :: forall ann. Report ann -> Doc ann
prettyReport r =
  pretty r.reportKind
    <> pretty '>'
    <> pretty r.shortDescription
    <> Pretty.nest
      2
      ( case r.descriptions of
          [] -> mempty
          _ ->
            Pretty.line
              <> Pretty.vsep (prettyLongDescription <$> r.descriptions)
      )

