module Octizys.FrontEnd.Format.TopItem where

import Control.Arrow ((<<<))
import Data.Text (Text)
import Octizys.Common.Format.Config (nest)
import qualified Octizys.Common.Format.Config as Format
import Octizys.FrontEnd.Cst.TopItem
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
import Octizys.FrontEnd.Format.Expression (formatDefinition)
import Prettyprinter (Doc, Pretty (pretty), vsep)
import qualified Prettyprinter as Pretty


formatImportItems
  :: Format.Configuration
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
  :: Format.Configuration
  -> ImportModule
  -> Doc ann
formatImportModule
  configuration
  (ImportModuleAs' {_import, path, alias}) =
    pretty @Text "import"
      <> (Pretty.group <<< nest configuration)
        ( Pretty.line
            <> pretty (snd path)
            <> maybe mempty (\x -> Pretty.line <> pretty x) alias
        )
formatImportModule
  configuration
  ( ImportModuleUnqualified'
      { _import
      , path
      , items
      }
    ) =
    pretty @Text "import"
      <> (Pretty.group <<< nest configuration)
        ( Pretty.line
            <> pretty (snd path)
            <> pretty '('
            <> Pretty.line
            <> nest configuration (formatImportItems configuration items)
            <> Pretty.line
            <> pretty ')'
        )


formatModule
  :: Pretty evar
  => Pretty tvar
  => Format.Configuration
  -> Module evar tvar
  -> Doc ann
formatModule
  configuration
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
              configuration
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
                configuration
                (fst x)
                <> pretty ';'
                <> Pretty.hardline
          )
            <$> _definitions
        )
