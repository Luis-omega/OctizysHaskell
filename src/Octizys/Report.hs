{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Octizys.Report where

import Data.Text (Text)
import Prettyprinter (Doc, Pretty (pretty))
import qualified Prettyprinter as Pretty
import Octizys.Pretty.Formatter (Formatter (format))
import Octizys.Pretty.FormatContext (FormatContext, nest)


data ReportKind
  = ReportError
  | ReportWarn
  | ReportInfo
  deriving (Show, Eq, Ord)


instance Formatter ann (FormatContext ann) ReportKind where
  format _ ReportError = pretty @Text "Error"
  format _ ReportWarn = pretty @Text "Warning"
  format _ ReportInfo = pretty @Text "Info"


data LongDescription ann = LongDescription'
  { preDescription :: Maybe Text
  , source :: Maybe (Doc ann)
  , afterDescription :: Maybe Text
  }

instance Formatter ann (FormatContext ann) (LongDescription ann) where
  format ctx ld =
    pretty ld.preDescription
      <> case source ld of
        Just content ->
          nest ctx
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
