{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Octizys.Package.Build where

import Data.Aeson (ToJSON)
import Data.Aeson.Types (Value)
import GHC.Generics (Generic, Generically (..))
import qualified Octizys.Compiler.Stage as Compiler
import Octizys.Format.Class (Formattable (format))
import qualified Octizys.Module.Build as Module
import qualified Octizys.Module.Index as Module
import qualified Octizys.Package.Index as Package
import qualified Octizys.Package.Reference as Package
import Prettyprinter (Pretty (pretty))
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


instance Pretty (Module.BuildState cs) => Pretty (BuildState cs) where
  pretty = prettyBuildState


instance Formattable (Module.Index cs) => Formattable (BuildState cs) where
  format c bs = format c bs.modules


getModuleIndex :: BuildState cs -> Module.Index cs
getModuleIndex = modules


transitionBuildState
  :: BuildState cs1
  -> Module.Index cs2
  -> BuildState cs2
transitionBuildState bs md = bs {modules = md}


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
