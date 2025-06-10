module Octizys.Pretty.Cst.TopItem where

import Control.Arrow ((<<<))
import Data.Text (Text)
import Octizys.Cst.TopItem
  ( ImportItems
      ( ImportItems'
      , items
      , lastComma
      , lastItem
      )
  , ImportModule
    ( ImportModuleAs'
    , ImportModuleUnqualified'
    , alias
    , items
    , path
    , _import
    )
  , Module (Module', definitions, imports, lastComments)
  )
import Octizys.Pretty.Cst.Expression (formatDefinition)
import Octizys.Pretty.FormatContext (FormatContext, nest)
import Prettyprinter (Doc, Pretty (pretty), vsep)
import qualified Prettyprinter as Pretty


formatImportItems
  :: FormatContext ann
  -> ImportItems
  -> Doc ann
formatImportItems
  _
  (ImportItems' {items, lastItem, lastComma}) =
    let lastI =
          maybe
            mempty
            ( const
                ( Pretty.line
                    <> pretty ','
                )
            )
            lastComma
     in case items of
          [] -> pretty lastItem <> lastI
          (start : end) ->
            pretty (fst start)
              <> (Pretty.vsep <<< Pretty.punctuate (pretty ','))
                (((\(x, _) -> pretty x) <$> end) ++ [pretty lastItem])
              <> lastI


formatImportModule
  :: FormatContext ann
  -> ImportModule
  -> Doc ann
formatImportModule
  ctx
  (ImportModuleAs' {_import, path, alias}) =
    pretty @Text "import"
      <> (Pretty.group <<< nest ctx)
        ( Pretty.line
            <> pretty (snd path)
            <> maybe mempty (\x -> Pretty.line <> pretty x) alias
        )
formatImportModule
  ctx
  ( ImportModuleUnqualified'
      { _import
      , path
      , items
      }
    ) =
    pretty @Text "import"
      <> (Pretty.group <<< nest ctx)
        ( Pretty.line
            <> pretty (snd path)
            <> pretty '('
            <> Pretty.line
            <> case items of
              Just its ->
                nest ctx (formatImportItems ctx its)
                  <> Pretty.line
              Nothing -> mempty
            <> pretty ')'
        )


formatModule
  :: ( FormatContext ann
       -> evar
       -> Doc ann
     )
  -> ( FormatContext ann
       -> tvar
       -> Doc ann
     )
  -> FormatContext ann
  -> Module evar tvar
  -> Doc ann
formatModule
  fmtEvar
  fmtTvar
  ctx
  ( Module'
      { lastComments = _lastComment
      , definitions = _definitions
      , imports = _imports
      }
    ) =
    -- TODO: missing last comment!
    vsep
      ( ( \x ->
            formatImportModule
              ctx
              (fst x)
              <> pretty ';'
              <> Pretty.hardline
        )
          <$> _imports
      )
      <> Pretty.hardline
      <> vsep
        ( ( \x ->
              formatDefinition
                fmtEvar
                fmtTvar
                ctx
                (fst x)
                <> pretty ';'
                <> Pretty.hardline
          )
            <$> _definitions
        )
