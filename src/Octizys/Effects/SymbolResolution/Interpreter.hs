{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Octizys.Effects.SymbolResolution.Interpreter
  ( runSymbolResolution
  , SymbolResolutionState
    ( SymbolResolutionState'
    , genInfoId
    , genVarExp
    , genVarType
    , expVarTable
    , typeVarTable
    , infoTable
    )
  , initialSymbolResolutionState
  , SourceInfo (SourceInfo', span, preComments, afterComment)
  , SourceExpressionVariableInfo (name, variableId, definitionSpan, typeId)
  , SourceTypeVariableInfo (name, variableId, definitionSpan)
  , runSymbolResolutionFull
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

import Octizys.Cst.Comment (Comment)
import Octizys.Cst.Expression (ExpressionVariableId (ExpressionVariableId'))
import Octizys.Cst.InfoId (InfoId (InfoId'))
import Octizys.Cst.Span (Span)
import Octizys.Cst.Type (TypeVariableId (TypeVariableId'))
import Octizys.Cst.VariableId (VariableId (VariableId'))
import Octizys.Effects.SymbolResolution.Effect
  ( SymbolResolution
      ( CreateExpressionVariable
      , CreateInformation
      , CreateTypeVariable
      )
  )
import Prelude hiding (span)


-- ============================================
-- Data Types
-- ============================================

data SourceInfo = SourceInfo'
  { span :: Span
  , preComments :: [Comment]
  , afterComment :: Maybe Comment
  }
  deriving (Show, Eq, Ord)


data SourceTypeVariableInfo = SourceTypeVariableInfo'
  { name :: Maybe Text
  , variableId :: TypeVariableId
  , definitionSpan :: Maybe Span
  }
  deriving (Show, Eq, Ord)


data SourceExpressionVariableInfo = SourceExpressionVariableInfo'
  { name :: Text
  , variableId :: ExpressionVariableId
  , definitionSpan :: Maybe Span
  , typeId :: TypeVariableId
  }
  deriving (Show, Eq, Ord)


data SymbolResolutionState = SymbolResolutionState'
  { genVarType :: Int
  , genVarExp :: Int
  , genInfoId :: Int
  , expVarTable :: Map ExpressionVariableId SourceExpressionVariableInfo
  , typeVarTable :: Map TypeVariableId SourceTypeVariableInfo
  , infoTable :: Map InfoId SourceInfo
  }


initialSymbolResolutionState :: SymbolResolutionState
initialSymbolResolutionState =
  SymbolResolutionState'
    { genVarType = 0
    , genVarExp = 0
    , genInfoId = 0
    , expVarTable = Map.empty
    , typeVarTable = Map.empty
    , infoTable = Map.empty
    }


-- ============================================
-- Helper functions
-- ============================================

createInfoId :: SymbolResolutionState -> (InfoId, SymbolResolutionState)
createInfoId s@SymbolResolutionState' {genInfoId} =
  (InfoId' genInfoId, s {genInfoId = genInfoId + 1})


createExpVarId
  :: SymbolResolutionState -> (ExpressionVariableId, SymbolResolutionState)
createExpVarId s@SymbolResolutionState' {genVarExp} =
  ( ExpressionVariableId' (VariableId' genVarExp)
  , s {genVarExp = genVarExp + 1}
  )


createTypeVarId
  :: SymbolResolutionState -> (TypeVariableId, SymbolResolutionState)
createTypeVarId s@SymbolResolutionState' {genVarType} =
  (TypeVariableId' (VariableId' genVarType), s {genVarType = genVarType + 1})


-- ============================================
-- Interpreter
-- ============================================

runSymbolResolution
  :: State SymbolResolutionState :> es
  => Eff (SymbolResolution : es) a
  -> Eff es a
runSymbolResolution = interpret \_ -> \case
  CreateExpressionVariable name mSpan -> do
    SymbolResolutionState' {..} <- get
    let (varId, s1) = createExpVarId SymbolResolutionState' {..}
        (typeId, s2) = createTypeVarId s1
        info =
          SourceExpressionVariableInfo'
            { name = name
            , variableId = varId
            , definitionSpan = mSpan
            , typeId = typeId
            }
    put
      s2
        { expVarTable = Map.insert varId info expVarTable
        , typeVarTable =
            Map.insert
              typeId
              (SourceTypeVariableInfo' Nothing typeId mSpan)
              typeVarTable
        }
    pure (varId, typeId)
  CreateTypeVariable mName mSpan -> do
    SymbolResolutionState' {..} <- get
    let (typeId, s1) = createTypeVarId SymbolResolutionState' {..}
        info =
          SourceTypeVariableInfo'
            { name = mName
            , variableId = typeId
            , definitionSpan = mSpan
            }
    put s1 {typeVarTable = Map.insert typeId info typeVarTable}
    pure typeId
  CreateInformation span pre after -> do
    SymbolResolutionState' {..} <- get
    let (infoId, s1) = createInfoId SymbolResolutionState' {..}
        info = SourceInfo' span pre after
    put s1 {infoTable = Map.insert infoId info infoTable}
    pure infoId


runSymbolResolutionFull
  :: SymbolResolutionState
  -> Eff (SymbolResolution : State SymbolResolutionState : es) a
  -> Eff es (a, SymbolResolutionState)
runSymbolResolutionFull s action = runState s $ runSymbolResolution action
