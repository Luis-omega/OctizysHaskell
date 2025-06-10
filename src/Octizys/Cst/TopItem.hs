{- | This module defines the items expected to be
present at the top of a file
-}
module Octizys.Cst.TopItem where

import Data.Text (Text)
import Octizys.Common.LogicPath (LogicPath)
import Octizys.Common.Name (Name)
import Octizys.Cst.Expression (Definition)
import Octizys.Cst.SourceInfo (SourceInfo, SourceVariable)
import Prettyprinter (Pretty (pretty))
import qualified Prettyprinter as Pretty


data ModulePath
  = ModuleLogicPath LogicPath
  | ModuleVarPath Name
  deriving (Show, Eq, Ord)


instance Pretty ModulePath where
  pretty (ModuleLogicPath l) = pretty l
  pretty (ModuleVarPath n) = pretty n


data ImportItem = ImportVariable
  { info :: SourceInfo
  , name :: Name
  }
  deriving (Show, Eq, Ord)


instance Pretty ImportItem where
  pretty (ImportVariable {name}) = pretty name


data ImportItems = ImportItems'
  { items :: [(ImportItem, SourceInfo)]
  , lastItem :: ImportItem
  , lastComma :: Maybe SourceInfo
  }
  deriving (Show, Eq, Ord)


data ImportAlias = ImportAlias'
  { _as :: SourceInfo
  , path :: (SourceInfo, ModulePath)
  }
  deriving (Show, Eq, Ord)


instance Pretty ImportAlias where
  pretty (ImportAlias' {_as, path}) =
    pretty @Text "as"
      <> Pretty.line
      <> pretty (snd path)


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
      , items :: Maybe ImportItems
      , rparen :: SourceInfo
      }
  deriving (Show, Eq, Ord)


data TopItem
  = TopDefinition
      { definition :: Definition SourceVariable SourceVariable
      , semicolon :: SourceInfo
      }
  | TopImport
      { importModule :: ImportModule
      , semicolon :: SourceInfo
      }
  deriving (Show, Eq, Ord)


data Module evar tvar = Module'
  { imports :: [(ImportModule, SourceInfo)]
  , definitions :: [(Definition evar tvar, SourceInfo)]
  -- ^ The @SourceInfo@ is for the colon
  , lastComments :: Maybe SourceInfo
  }
  deriving (Show, Eq, Ord)
