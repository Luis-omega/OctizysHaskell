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
  , SourceExpressionVariableInfo
    ( name
    , expressionVariableId
    , expDefinitionSpan
    , typeId
    )
  , SourceTypeVariableInfo
    ( name
    , typeVariableId
    , typeDefinitionSpan
    )
  , runSymbolResolutionFull
  , SymbolResolutionError
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local

import qualified Data.Map as Map
import Data.Text (Text)

import Effectful.Error.Static (Error, throwError)
import Octizys.Cst.Expression (ExpressionVariableId (ExpressionVariableId'))
import Octizys.Cst.InfoId (InfoId (InfoId'))
import Octizys.Cst.Span (Span)
import Octizys.Cst.Type (TypeVariableId (TypeVariableId'))
import Octizys.Cst.VariableId (VariableId (VariableId'))
import Octizys.Effects.SymbolResolution.Effect
  ( SourceExpressionVariableInfo
      ( SourceExpressionVariableInfo'
      , expDefinitionSpan
      , expressionVariableId
      , name
      , typeId
      )
  , SourceTypeVariableInfo (name, typeDefinitionSpan, typeVariableId, SourceTypeVariableInfo')
  , SymbolResolution
    ( CreateInformation
    , DefinitionOfExpressionVariable
    , DefinitionOfTypeVariable
    , FoundExpressionVariable
    , FoundTypeVariable
    , RemoveExpressionDefinition
    , RemoveTypeDefinition, GetSymbolResolutionState, PutSymbolResolutionState
    )
  , SymbolResolutionState
    ( SymbolResolutionState'
    , expNamesToId
    , expVarTable
    , genInfoId
    , genVarExp
    , genVarType
    , infoTable
    , typeNamesToId
    , typeVarTable
    ), SourceInfo (SourceInfo', span, afterComment, preComments)
  )
import Octizys.HistoryMap
  ( empty
  , lookup
  , popValue
  , pushValue
  )
import Prelude hiding (lookup, span)


-- ============================================
-- Data Types
-- ============================================

data SymbolResolutionError
  = CantFindExpVariableThatMustBeIn ExpressionVariableId Text
  | CantFindTypeVariableThatMustBeIn TypeVariableId Text
  | AttemptToDeleteAnonymousVariable SourceTypeVariableInfo
  deriving (Show, Eq, Ord)


initialSymbolResolutionState :: SymbolResolutionState
initialSymbolResolutionState =
  SymbolResolutionState'
    { genVarType = 0
    , genVarExp = 0
    , genInfoId = 0
    , expVarTable = Map.empty
    , typeVarTable = Map.empty
    , infoTable = Map.empty
    , expNamesToId = empty
    , typeNamesToId = empty
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


registerNewExpressionVariable
  :: Text
  -- ^ The name of the variable
  -> Maybe Span
  -- ^ The span where it is defined if know.
  -> SymbolResolutionState
  -> ( ExpressionVariableId
     , SymbolResolutionState
     )
registerNewExpressionVariable name maybeSpan st =
  let (varId, s1) = createExpVarId st
      (typeId, s2) = createTypeVarId s1
      info =
        SourceExpressionVariableInfo'
          { name = name
          , expressionVariableId = varId
          , expDefinitionSpan = maybeSpan
          , typeId = typeId
          }
      newExpNamesToId =
        pushValue (name, varId) (expNamesToId s2)
      stOut =
        s2
          { expVarTable = Map.insert varId info (expVarTable s2)
          , typeVarTable =
              Map.insert
                typeId
                (SourceTypeVariableInfo' Nothing typeId Nothing)
                (typeVarTable s2)
          , expNamesToId = newExpNamesToId
          }
   in (varId, stOut)


registerNewTypeVariable
  :: Maybe Text
  -- ^ The name of the variable
  -> Maybe Span
  -- ^ The span where it is defined if know.
  -> SymbolResolutionState
  -> ( TypeVariableId
     , SymbolResolutionState
     )
registerNewTypeVariable maybeName maybeSpan st =
  let
    (typeId, s2) = createTypeVarId st
    newTypeNamesToId =
      case maybeName of
        Just name ->
          pushValue (name, typeId) (typeNamesToId s2)
        Nothing -> typeNamesToId s2
    stOut =
      s2
        { typeVarTable =
            Map.insert
              typeId
              ( SourceTypeVariableInfo'
                  Nothing
                  typeId
                  maybeSpan
              )
              (typeVarTable s2)
        , typeNamesToId = newTypeNamesToId
        }
   in
    (typeId, stOut)


-- ============================================
-- Interpreter
-- ============================================

runSymbolResolution
  :: State SymbolResolutionState :> es
  => Error SymbolResolutionError :> es
  => Eff (SymbolResolution : es) a
  -> Eff es a
runSymbolResolution = interpret \_ -> \case
  FoundExpressionVariable name -> do
    originalState <- get
    let maybeId = lookup name (expNamesToId originalState)
    case maybeId of
      Just expId ->
        pure expId
      Nothing ->
        let (var, newState) =
              registerNewExpressionVariable
                name
                Nothing
                originalState
         in do
              put newState
              pure var
  FoundTypeVariable name -> do
    originalState <- get
    let maybeInfo = lookup name (typeNamesToId originalState)
    case maybeInfo of
      Just tyInfo -> pure tyInfo
      Nothing ->
        let (var, newState) =
              registerNewTypeVariable
                (Just name)
                Nothing
                originalState
         in do
              put newState
              pure var
  DefinitionOfExpressionVariable name varSpan -> do
    originalState <- get
    let maybeId = lookup name (expNamesToId originalState)
    case maybeId of
      -- There is a variable with that name in the current scope.
      -- We need to determine if we need to create one (newScope)
      -- or to update the definition field.
      Just expId ->
        case Map.lookup expId (expVarTable originalState) of
          Just expInfo ->
            case expInfo.expDefinitionSpan of
              --  We need to create a new variable
              -- and register it.
              Just _ ->
                let (var, newState) = registerNewExpressionVariable name (Just varSpan) originalState
                 in do
                      put newState
                      pure var
              --  A variable was found before it's definition
              -- (maybe in a `where` clause)
              Nothing ->
                let newInfo = expInfo {expDefinitionSpan = Just varSpan}
                    newState =
                      originalState
                        { expVarTable =
                            Map.insert expId newInfo (expVarTable originalState)
                        }
                 in do
                      put newState
                      pure expId
          -- We found a variable in the map but no information
          -- is associated with it, this is a bug!
          Nothing ->
            throwError
              (CantFindExpVariableThatMustBeIn expId name)
      Nothing ->
        let (var, newState) =
              registerNewExpressionVariable
                name
                (Just varSpan)
                originalState
         in do
              put newState
              pure var
  DefinitionOfTypeVariable name varSpan -> do
    originalState <- get
    let maybeId = lookup name (typeNamesToId originalState)
    case maybeId of
      -- There is a variable with that name in the current scope.
      -- We need to determine if we need to create one (newScope)
      -- or to update the definition field.
      Just typeId ->
        case Map.lookup typeId (typeVarTable originalState) of
          Just typeInfo ->
            case typeInfo.typeDefinitionSpan of
              --  We need to create a new variable
              -- and register it.
              Just _ ->
                let (var, newState) = registerNewTypeVariable (Just name) (Just varSpan) originalState
                 in do
                      put newState
                      pure var
              --  A variable was found before it's definition
              -- (maybe in a `where` clause)
              Nothing ->
                let newInfo =
                      typeInfo {typeDefinitionSpan = Just varSpan}
                    newState =
                      originalState
                        { typeVarTable =
                            Map.insert typeId newInfo (typeVarTable originalState)
                        }
                 in do
                      put newState
                      pure typeId
          -- We found a variable in the map but no information
          -- is associated with it, this is a bug!
          Nothing ->
            throwError
              (CantFindTypeVariableThatMustBeIn typeId name)
      Nothing ->
        let (var, newState) =
              registerNewTypeVariable
                (Just name)
                (Just varSpan)
                originalState
         in do
              put newState
              pure var
  RemoveExpressionDefinition expId ->
    modify $ \s ->
      case Map.lookup expId s.expVarTable of
        Just expInfo ->
          s {expNamesToId = popValue expInfo.name s.expNamesToId}
        Nothing -> s
  RemoveTypeDefinition typeId -> do
    s <- get
    case Map.lookup typeId s.typeVarTable of
      Just typeInfo ->
        case typeInfo.name of
          Just tName ->
            put s {typeNamesToId = popValue tName s.typeNamesToId}
          -- A bug, a type without a name is a internal thing
          -- that the parser shouldn't touch!
          Nothing -> throwError $ AttemptToDeleteAnonymousVariable typeInfo
      Nothing -> pure ()
  CreateInformation span pre after -> do
    SymbolResolutionState' {..} <- get
    let (infoId, s1) = createInfoId SymbolResolutionState' {..}
        info = SourceInfo' span pre after
    put s1 {infoTable = Map.insert infoId info infoTable}
    pure infoId
  GetSymbolResolutionState -> get
  PutSymbolResolutionState s -> put s


runSymbolResolutionFull
  :: Error SymbolResolutionError :> es
  => SymbolResolutionState
  -> Eff (SymbolResolution : State SymbolResolutionState : es) a
  -> Eff es (a, SymbolResolutionState)
runSymbolResolutionFull s action = runState s $ runSymbolResolution action

