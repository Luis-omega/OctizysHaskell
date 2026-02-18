{- | This module defines the items expected to be
present at the top of a file
-}
module Octizys.FrontEnd.Cst.TopItem where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Octizys.Classes.From (From (from))
import Octizys.Common.LogicPath (LogicPath)
import Octizys.Common.Name (Name)
import Octizys.FrontEnd.Cst.Expression (Definition)
import Octizys.FrontEnd.Cst.SourceInfo (SourceInfo, SourceVariable)
import Prettyprinter (Doc, Pretty (pretty))
import qualified Prettyprinter as Pretty

import Control.Arrow ((<<<))
import Data.Aeson (ToJSON)
import GHC.Generics (Generic, Generically (..))
import Octizys.Format.Class (Formattable (format))
import qualified Octizys.Format.Config as Format
import qualified Octizys.Format.Utils as Format


data ModulePath
  = ModuleLogicPath LogicPath
  | ModuleVarPath Name
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically ModulePath


instance Pretty ModulePath where
  pretty (ModuleLogicPath l) = pretty l
  pretty (ModuleVarPath n) = pretty n


instance From LogicPath ModulePath where
  from (ModuleLogicPath l) = l
  from (ModuleVarPath n) = from (n :| [])


data ImportItem = ImportVariable
  { info :: SourceInfo
  , name :: Name
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically ImportItem


instance Pretty ImportItem where
  pretty (ImportVariable {name}) = pretty name


data ImportItems = ImportItems'
  { items :: [(ImportItem, SourceInfo)]
  , lastItem :: ImportItem
  , lastComma :: Maybe SourceInfo
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically ImportItems


instance From (NonEmpty (SourceInfo, Name)) ImportItems where
  from (ImportItems' {items, lastItem}) =
    (lastItem.info, lastItem.name) :| ((\(x, _) -> (x.info, x.name)) <$> items)


instance Formattable ImportItems where
  format = formatImportItems


data ImportAlias = ImportAlias'
  { _as :: SourceInfo
  , path :: (SourceInfo, ModulePath)
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically ImportAlias


instance Pretty ImportAlias where
  pretty (ImportAlias' {_as, path}) =
    pretty @Text "as"
      <> Pretty.line
      <> pretty (snd path)


instance From LogicPath ImportAlias where
  from (ImportAlias' _ (_, p)) = from p


data ImportModule
  = ImportModuleAs'
      { _import :: SourceInfo
      , path :: (SourceInfo, ModulePath)
      , alias :: Maybe ImportAlias
      }
  | ImportModuleUnqualified'
      { _import :: SourceInfo
      , unqualified :: SourceInfo
      , path :: (SourceInfo, ModulePath)
      , lparen :: SourceInfo
      , items :: ImportItems
      , rparen :: SourceInfo
      }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically ImportModule


instance Formattable ImportModule where
  format = formatImportModule


data TopItem
  = TopDefinition
      { definition :: Definition SourceVariable SourceVariable
      , semicolon :: SourceInfo
      }
  | TopImport
      { importModule :: ImportModule
      , semicolon :: SourceInfo
      }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically TopItem


data Module evar tvar = Module'
  { systemPath :: FilePath
  , logicPath :: LogicPath
  , imports :: [(ImportModule, SourceInfo)]
  , definitions :: [(Definition evar tvar, SourceInfo)]
  -- ^ The @SourceInfo@ is for the colon
  , lastComments :: Maybe SourceInfo
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (Module evar tvar)


instance
  (Formattable evar, Formattable tvar)
  => Formattable (Module evar tvar)
  where
  format = formatModule


-- * Format


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
      <> (Pretty.group <<< Format.nest configuration)
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
      <> (Pretty.group <<< Format.nest configuration)
        ( Pretty.line
            <> pretty (snd path)
            <> pretty '('
            <> Pretty.line
            <> Format.nest configuration (formatImportItems configuration items)
            <> Pretty.line
            <> pretty ')'
        )


formatModule
  :: Formattable evar
  => Formattable tvar
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
    Pretty.vsep
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
      <> Pretty.vsep
        ( ( \x ->
              format
                configuration
                (fst x)
                <> pretty ';'
                <> Pretty.hardline
          )
            <$> _definitions
        )
