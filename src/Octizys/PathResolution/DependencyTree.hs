module Octizys.PathResolution.DependencyTree
  ( DependencyTree
  , makeEmptyDependencyTree
  )
where

import Octizys.PathResolution.PathIndex (PathIndex, RootPaths)
import Prettyprinter (Pretty (pretty))

import Data.Aeson (ToJSON)
import GHC.Generics (Generic, Generically (..))


-- TODO:STUB
data DependencyTree = DependencyTree'
  { rootPaths :: RootPaths
  , pathIndex :: PathIndex
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically DependencyTree


-- TODO: find a better instance!
instance Pretty DependencyTree where
  pretty (DependencyTree' _ _) = ""


-- TODO:STUB
makeEmptyDependencyTree :: RootPaths -> PathIndex -> DependencyTree
makeEmptyDependencyTree rp pi = DependencyTree' rp pi
