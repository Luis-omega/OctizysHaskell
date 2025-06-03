module Octizys.SymbolResolution where

import Control.Monad (forM)
import Data.Map (Map)
import Effectful (Eff, (:>))
import Effectful.State.Static.Local (State, get, modify)
import Octizys.Common.HistoryMap (HistoryMap)
import qualified Octizys.Common.HistoryMap as HistoryMap
import Octizys.Common.Id
  ( ExpressionVariableId
  , TypeVariableId
  )
import Octizys.Cst.Expression (Expression)
import Octizys.Cst.SourceInfo (SourceInfo, SourceVariable)
import Octizys.Cst.Type
  ( Type
      ( Arrow
      , BoolType
      , IntType
      , Parens
      , TVariable
      , info
      , lparen
      , remain
      , rparen
      , start
      , variable
      , _type
      )
  )
import Octizys.Effects.IdGenerator.Interpreter (IdGenerator, generateId)


data Context = Context'
  { evarNames :: HistoryMap SourceVariable ExpressionVariableId
  , tvarNames :: HistoryMap SourceVariable TypeVariableId
  , evarMap :: Map TypeVariableId SourceVariable
  , tvarMap :: Map TypeVariableId SourceVariable
  }


data SymbolResolutionWarning
  = EVarShadowing SourceVariable ExpressionVariableId
  | TVarShadowing SourceVariable TypeVariableId
  deriving (Show, Eq, Ord)


data SymbolResolutionError
  = UnboundTypeVariable SourceVariable SourceInfo
  | UnboundExpVariable SourceVariable SourceInfo
  deriving (Show, Eq, Ord)


data SymbolResolutionState = SymbolResolutionState'
  { context :: Context
  , warnings :: [SymbolResolutionWarning]
  , errors :: [SymbolResolutionError]
  }


lookupEvar
  :: State SymbolResolutionState :> es
  => SourceVariable
  -> Eff es (Maybe ExpressionVariableId)
lookupEvar var = do
  s <- get
  let result = HistoryMap.lookup var s.context.evarNames
  pure result


lookupTvar
  :: State SymbolResolutionState :> es
  => SourceVariable
  -> Eff es (Maybe TypeVariableId)
lookupTvar var = do
  s <- get
  let result = HistoryMap.lookup var s.context.tvarNames
  pure result


tellWarn
  :: State SymbolResolutionState :> es
  => SymbolResolutionWarning
  -> Eff es ()
tellWarn w = modify (\s -> s {warnings = w : s.warnings})


tellError
  :: State SymbolResolutionState :> es
  => SymbolResolutionError
  -> Eff es ()
tellError w = modify (\s -> s {errors = w : s.errors})


{- | To use to translate a type with source variables
to a type with unique ids.
This shouldn't handle scopes, only lookup for type variables.
Is responsibility of `makeVariablesUnique` to add them
to the scope at scheme places or if is a global type, to
be added to global scope before calling this function.
-}
makeTypeVariablesUnique
  :: State SymbolResolutionState :> es
  => IdGenerator TypeVariableId :> es
  => Type SourceVariable
  -> Eff es (Type TypeVariableId)
makeTypeVariablesUnique t =
  case t of
    BoolType {info} -> pure BoolType {info = info}
    IntType {info} -> pure IntType {info = info}
    Arrow {start, remain} -> do
      startNew <- makeTypeVariablesUnique start
      remainNew <-
        forM
          remain
          ( \(x, y) ->
              do
                result <- makeTypeVariablesUnique y
                pure (x, result)
          )
      pure Arrow {start = startNew, remain = remainNew}
    Parens {lparen, rparen, _type} -> do
      newType <- makeTypeVariablesUnique _type
      pure Parens {lparen, rparen, _type = newType}
    TVariable {info, variable} -> do
      maybeId <- lookupTvar variable
      case maybeId of
        Just tid -> pure TVariable {info, variable = tid}
        Nothing -> do
          tellError (UnboundTypeVariable variable info)
          newId <- generateId
          pure
            TVariable
              { info
              , variable = newId
              }


makeVariablesUnique
  :: State SymbolResolutionState :> es
  => Expression SourceVariable SourceVariable
  -> Eff es ()
makeVariablesUnique = undefined
