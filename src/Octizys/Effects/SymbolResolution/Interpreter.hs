{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Octizys.Effects.SymbolResolution.Interpreter
  ( runSymbolResolution
  , SymbolResolutionState
    ( SymbolResolutionState'
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
import Octizys.Common.HistoryMap
  ( empty
  , lookup
  , popValue
  , pushValue
  )
import Octizys.Cst.Expression (ExpressionVariableId)
import Octizys.Cst.Span (Span)
import Octizys.Cst.Type (InfoId, TypeVariableId)
import Octizys.Effects.IdGenerator.Effect (IdGenerator, generateId)
import Octizys.Effects.SymbolResolution.Effect
  ( SourceExpressionVariableInfo
      ( SourceExpressionVariableInfo'
      , expDefinitionSpan
      , expressionVariableId
      , name
      , typeId
      )
  , SourceInfo (SourceInfo', afterComment, preComments, span)
  , SourceTypeVariableInfo
    ( SourceTypeVariableInfo'
    , name
    , typeDefinitionSpan
    , typeVariableId
    )
  , SymbolResolution
    ( CreateInformation
    , DefinitionOfExpressionVariable
    , DefinitionOfTypeVariable
    , FoundExpressionVariable
    , FoundTypeVariable
    , GetSymbolResolutionState
    , PutSymbolResolutionState
    , RemoveExpressionDefinition
    , RemoveTypeDefinition
    )
  , SymbolResolutionState
    ( SymbolResolutionState'
    , expNamesToId
    , expVarTable
    , infoTable
    , typeNamesToId
    , typeVarTable
    )
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
    { expVarTable = Map.empty
    , typeVarTable = Map.empty
    , infoTable = Map.empty
    , expNamesToId = empty
    , typeNamesToId = empty
    }


-- ============================================
-- Helper functions
-- ============================================

createInfoId
  :: IdGenerator InfoId :> es
  => Eff es InfoId
createInfoId = generateId


createExpVarId
  :: IdGenerator ExpressionVariableId :> es
  => Eff es ExpressionVariableId
createExpVarId = generateId


createTypeVarId
  :: IdGenerator TypeVariableId :> es
  => Eff es TypeVariableId
createTypeVarId = generateId


registerNewExpressionVariable
  :: IdGenerator TypeVariableId :> es
  => IdGenerator ExpressionVariableId :> es
  => Text
  -- ^ The name of the variable
  -> Maybe Span
  -- ^ The span where it is defined if know.
  -> SymbolResolutionState
  -> Eff
      es
      ( ExpressionVariableId
      , SymbolResolutionState
      )
registerNewExpressionVariable name maybeSpan st = do
  varId <- createExpVarId
  typeId <- createTypeVarId
  let info =
        SourceExpressionVariableInfo'
          { name = name
          , expressionVariableId = varId
          , expDefinitionSpan = maybeSpan
          , typeId = typeId
          }
      newExpNamesToId =
        pushValue (name, varId) (expNamesToId st)
      stOut =
        st
          { expVarTable = Map.insert varId info (expVarTable st)
          , typeVarTable =
              Map.insert
                typeId
                (SourceTypeVariableInfo' Nothing typeId Nothing)
                (typeVarTable st)
          , expNamesToId = newExpNamesToId
          }
  pure (varId, stOut)


registerNewTypeVariable
  :: IdGenerator TypeVariableId :> es
  => Maybe Text
  -- ^ The name of the variable
  -> Maybe Span
  -- ^ The span where it is defined if know.
  -> SymbolResolutionState
  -> Eff
      es
      ( TypeVariableId
      , SymbolResolutionState
      )
registerNewTypeVariable maybeName maybeSpan st = do
  typeId <- createTypeVarId
  let
    newTypeNamesToId =
      case maybeName of
        Just name ->
          pushValue (name, typeId) (typeNamesToId st)
        Nothing -> typeNamesToId st
    stOut =
      st
        { typeVarTable =
            Map.insert
              typeId
              ( SourceTypeVariableInfo'
                  Nothing
                  typeId
                  maybeSpan
              )
              (typeVarTable st)
        , typeNamesToId = newTypeNamesToId
        }
  pure (typeId, stOut)


-- ============================================
-- Interpreter
-- ============================================

runSymbolResolution
  :: State SymbolResolutionState :> es
  => Error SymbolResolutionError :> es
  => IdGenerator TypeVariableId :> es
  => IdGenerator ExpressionVariableId :> es
  => IdGenerator InfoId :> es
  => Eff (SymbolResolution : es) a
  -> Eff es a
runSymbolResolution = interpret \_ -> \case
  FoundExpressionVariable name -> do
    originalState <- get
    let maybeId = lookup name (expNamesToId originalState)
    case maybeId of
      Just expId ->
        pure expId
      Nothing -> do
        (var, newState) <-
          registerNewExpressionVariable
            name
            Nothing
            originalState
        put newState
        pure var
  FoundTypeVariable name -> do
    originalState <- get
    let maybeInfo = lookup name (typeNamesToId originalState)
    case maybeInfo of
      Just tyInfo -> pure tyInfo
      Nothing -> do
        (var, newState) <-
          registerNewTypeVariable
            (Just name)
            Nothing
            originalState
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
              Just _ -> do
                (var, newState) <-
                  registerNewExpressionVariable
                    name
                    (Just varSpan)
                    originalState
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
      Nothing -> do
        (var, newState) <-
          registerNewExpressionVariable
            name
            (Just varSpan)
            originalState
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
              Just _ -> do
                (var, newState) <-
                  registerNewTypeVariable
                    (Just name)
                    (Just varSpan)
                    originalState
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
      Nothing -> do
        (var, newState) <-
          registerNewTypeVariable
            (Just name)
            (Just varSpan)
            originalState
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
    st <- get
    infoId <- createInfoId
    let info = SourceInfo' span pre after
    put st {infoTable = Map.insert infoId info st.infoTable}
    pure infoId
  GetSymbolResolutionState -> get
  PutSymbolResolutionState s -> put s


runSymbolResolutionFull
  :: Error SymbolResolutionError :> es
  => IdGenerator TypeVariableId :> es
  => IdGenerator ExpressionVariableId :> es
  => IdGenerator InfoId :> es
  => SymbolResolutionState
  -> Eff (SymbolResolution : State SymbolResolutionState : es) a
  -> Eff es (a, SymbolResolutionState)
runSymbolResolutionFull s action = runState s $ runSymbolResolution action
