{- | This module defines the items expected to be
present at the top of a file
-}
module Octizys.Cst.TopItem where

import Octizys.Common.LogicPath (LogicPath)
import Octizys.Common.Name (Name)
import Octizys.Cst.Expression (Definition)
import Octizys.Cst.SourceInfo (SourceInfo, SourceVariable)


data ModulePath
  = ModuleLogicPath LogicPath
  | ModuleVarPath Name
  deriving (Show, Eq, Ord)


data ImportItem = ImportVariable
  { info :: SourceInfo
  , name :: Name
  }
  deriving (Show, Eq, Ord)


data ImportAlias = ImportAlias'
  { _as :: SourceInfo
  , path :: (SourceInfo, ModulePath)
  }
  deriving (Show, Eq, Ord)


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
      , items :: [ImportItem]
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
