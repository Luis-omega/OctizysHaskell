{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Octizys.Compiler.Package.Build where

import Data.Aeson (ToJSON)
import Data.Aeson.Types (Value)
import GHC.Generics (Generic, Generically (..))
import qualified Octizys.Compiler.Module.Build as Module
import qualified Octizys.Compiler.Module.Index as Module
import qualified Octizys.Compiler.Package.Index as Package
import qualified Octizys.Compiler.Package.Reference as Package
import qualified Octizys.Compiler.Stage as Compiler
import Prettyprinter (Pretty)
import qualified Prettyprinter as Pretty


data DependencyTree = DependencyTree'
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically DependencyTree


makeEmptyDependencyTree :: DependencyTree
makeEmptyDependencyTree = DependencyTree'


data BuildState (cs :: Compiler.Stage) = BuildState'
  { modules :: Module.Index cs
  , deps :: DependencyTree
  , info :: Package.Reference
  , packageDependencies :: Package.Index
  }
  deriving (Show, Eq, Ord, Generic)


makeBuildState
  :: Module.Index cs
  -> DependencyTree
  -> Package.Reference
  -> Package.Index
  -> BuildState cs
makeBuildState = BuildState'


prettyBuildState
  :: Pretty (Module.BuildState cs)
  => BuildState cs
  -> Pretty.Doc ann
prettyBuildState bs =
  Module.prettyIndex bs.modules


toJSONBuildState
  :: ToJSON (Module.BuildState cs) => BuildState cs -> Value
toJSONBuildState bs =
  Module.toJSONIndex bs.modules
