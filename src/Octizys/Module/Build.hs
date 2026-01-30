{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Octizys.Module.Build where

import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), object)
import Data.Text (Text)
import GHC.Generics (Generic, Generically (..))
import Octizys.Common.Format.Config (defaultConfiguration)
import Octizys.Common.Id (ExpressionVariableId, TypeVariableId)
import Octizys.Common.LogicPath (LogicPath)
import qualified Octizys.Compiler.Stage as Compiler
import qualified Octizys.Compiler.Stage as Compiler.Stage
import Octizys.FrontEnd.Cst.SourceInfo (SourceVariable)
import qualified Octizys.FrontEnd.Cst.TopItem as Cst
import qualified Octizys.FrontEnd.Format.TopItem as Cst
import Prettyprinter (Pretty (pretty))


data AstModule = AstModule'
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically AstModule


instance Pretty AstModule where
  pretty _ = pretty @Text "AstModuleStub"


data BuildState (cs :: Compiler.Stage) where
  Parsed
    :: Cst.Module SourceVariable SourceVariable
    -> BuildState Compiler.Stage.Parsed
  SymbolsSolved
    :: BuildState Compiler.Stage.Parsed
    -> Cst.Module ExpressionVariableId TypeVariableId
    -> BuildState Compiler.Stage.SymbolsSolved
  TypesChecked
    :: BuildState Compiler.Stage.SymbolsSolved
    -> AstModule
    -> BuildState Compiler.Stage.TypesChecked
  Optimized
    :: BuildState Compiler.Stage.TypesChecked
    -> AstModule
    -> BuildState Compiler.Stage.Optimized


deriving instance Show (BuildState cs)
deriving instance Eq (BuildState cs)
deriving instance Ord (BuildState cs)


instance ToJSON (BuildState Compiler.Stage.Parsed) where
  toJSON (Parsed m) =
    object
      [ "stage" .= ("Parsed" :: Text)
      , "module" .= m
      ]


instance Pretty (BuildState Compiler.Stage.Parsed) where
  pretty (Parsed m) = Cst.formatModule defaultConfiguration m


instance Pretty (BuildState Compiler.Stage.SymbolsSolved) where
  pretty (SymbolsSolved _ m) = Cst.formatModule defaultConfiguration m


instance Pretty (BuildState Compiler.Stage.TypesChecked) where
  pretty (TypesChecked _ m) = pretty m


instance Pretty (BuildState Compiler.Stage.Optimized) where
  pretty (Optimized _ m) = pretty m
