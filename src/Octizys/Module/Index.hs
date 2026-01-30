{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Octizys.Module.Index where

import Data.Aeson (ToJSON (toJSON), Value)
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics (Generic, Generically (..))
import Octizys.Common.LogicPath (LogicPath)
import qualified Octizys.Compiler.Stage as Compiler
import Octizys.Module.Build (BuildState)
import Prettyprinter (Pretty (pretty), (<+>))
import qualified Prettyprinter as Pretty


{- | Contains the information of both the absolute file path of a module
in the file system and the logicPath relative to the package it lives in.
-}
data Path = ModulePath'
  { filePath :: FilePath
  , logicPath :: LogicPath
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically Path


getFilePath :: Path -> FilePath
getFilePath = filePath


getLogicPath :: Path -> LogicPath
getLogicPath = logicPath


instance Pretty Path where
  pretty (ModulePath' _ lp) = pretty lp


newtype Index (cs :: Compiler.Stage) = Index'
  { unIndex :: Map LogicPath (BuildState cs)
  }
  deriving (Show, Eq, Ord, Generic)


toJSONIndex :: ToJSON (BuildState cs) => Index cs -> Value
toJSONIndex (Index' m) =
  toJSON $
    Map.map
      toJSON
      m


prettyIndex
  :: Pretty (BuildState cs)
  => Index cs
  -> Pretty.Doc ann
prettyIndex (Index' m) =
  Pretty.vsep
    [ pretty path
      <+> "=>"
      <> Pretty.line
      <> Pretty.indent 2 (pretty modl)
    | (path, modl) <- Map.toList m
    ]


empty :: Index Compiler.Parsed
empty = Index' mempty
