{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Octizys.Module.Build where

import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), object)
import Data.Text (Text)
import GHC.Generics (Generic, Generically (..))
import Octizys.Common.Id (ExpressionVariableId, TypeVariableId)
import qualified Octizys.Compiler.Stage as Compiler
import qualified Octizys.Compiler.Stage as Compiler.Stage
import Octizys.Format.Class (Formattable (format))
import Octizys.FrontEnd.Cst.SourceInfo (SourceVariable)
import qualified Octizys.FrontEnd.Cst.TopItem as Cst
import Prettyprinter (Pretty (pretty))


data AstModule = AstModule'
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically AstModule


instance Pretty AstModule where
  pretty _ = pretty @Text "AstModuleStub"


instance Formattable AstModule where
  format _ = pretty


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


instance Formattable (BuildState Compiler.Stage.Parsed) where
  format c (Parsed m) = format c m


instance Formattable (BuildState Compiler.Stage.SymbolsSolved) where
  format c (SymbolsSolved _ m) = format c m


instance Formattable (BuildState Compiler.Stage.TypesChecked) where
  format c (TypesChecked _ m) = format c m


instance Formattable (BuildState Compiler.Stage.Optimized) where
  format c (Optimized _ m) = format c m
