module Octizys.Compiler.StateSync where

import Effectful (Eff, (:>))

import qualified Octizys.Effects.SymbolResolution.Effect as SRS
import qualified Octizys.Inference.ConstraintsGeneration as Inference

import Data.Map (Map)
import Effectful.State.Static.Local (State, get, gets, put)
import qualified Octizys.Ast.Expression as Ast
import Octizys.Effects.Logger.Effect (Logger)
import Octizys.Effects.SymbolResolution.Effect
  ( SymbolResolution
  , putSymbolResolutionState
  )

import Control.Monad (forM_)
import qualified Data.Map as Map
import Octizys.Ast.Expression (buildDefinitionsMap)
import Octizys.Cst.Expression (ExpressionVariableId)


-- | The Evaluator needs a list of all defined symbols, this
-- avoid us handling scopes.
-- All variables have unique names and we already ran symbol
-- resolution, it means that we can use safely this map.
newtype DefinedSymbols = DefinedSymbols'
  { definedSymbols :: Map ExpressionVariableId Ast.Expression
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Semigroup
    , Monoid
    )
    via (Map ExpressionVariableId Ast.Expression)


-- | Sync the `InferenceState` based on the `SymbolResolution` state.
updateInferenceState
  :: SymbolResolution :> es
  => State Inference.InferenceState :> es
  => Eff es ()
updateInferenceState = do
  currentSymbolState <- SRS.getSymbolResolutionState
  currentInference <- get
  put
    currentInference
      { Inference.expVarTable = SRS.expVarTable currentSymbolState
      , Inference.realVariablesMax =
          SRS.genVarType currentSymbolState + 1
      , Inference.constraintMap =
          currentInference.constraintMap
            { Inference.nextTypeVar = SRS.genVarType currentSymbolState + 1
            }
      }


-- | Sync the `SymbolResolution` based on the `InferenceState` state.
updateSymbolState
  :: SymbolResolution :> es
  => State Inference.InferenceState :> es
  => Eff es ()
updateSymbolState = do
  currentInference <- get
  currentSymbolState <- SRS.getSymbolResolutionState
  putSymbolResolutionState
    currentSymbolState
      { SRS.expVarTable = Inference.expVarTable currentInference
      , SRS.genVarType =
          currentInference.constraintMap.nextTypeVar
            + 1
      }

-- | Add all the expression variables defined inside a
-- expression and add it's definitions to the `DefinedSymbols` state.
addExpressionSymbols
  :: State DefinedSymbols :> es
  => Ast.Expression
  -> Eff es (Map ExpressionVariableId Ast.Expression)
addExpressionSymbols expr = do
  mp <- gets definedSymbols
  let exprSymbols = buildDefinitionsMap expr
      newMap = Map.union mp exprSymbols
  put $ DefinedSymbols' newMap
  pure newMap


-- | Add all the expression variables defined in the right side
-- of a definition and then the definition itself to the
-- `DefinedSymbols` state.
addDefinedSymbol
  :: State DefinedSymbols :> es
  => State Inference.InferenceState :> es
  => Logger :> es
  => Ast.Definition
  -> Eff es (Map ExpressionVariableId Ast.Expression)
addDefinedSymbol d = do
  mp <- addExpressionSymbols d.definition
  let addedSymbol = Map.insert d.name d.definition mp
  forM_
    (Map.toList addedSymbol)
    ( \(nam, ex) ->
        Inference.insertKnowType nam (Ast.getType ex)
    )
  let newMap = Map.insert d.name d.definition addedSymbol
  put $ DefinedSymbols' newMap
  pure newMap

