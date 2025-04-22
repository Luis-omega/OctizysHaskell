{-# LANGUAGE FlexibleContexts #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Inference
  ( InferenceError
  , InferenceExpressionVar (InferenceExpressionVarC)
  , InferenceTypeVar (InferenceTypeVarC)
  , InferenceExpression
  , InferenceType
  , InferenceTopItem
  , TranslationState
  , TranslationError
  , TranslationWarrning
  , buildInferenceContext
  , transformType
  , transform
  , emptyState
  )
where

import Ast
  ( Expression
      ( Annotation
      , Application
      , Function
      , If
      , Let
      , LiteralExpression
      , Variable
      , annotationExpression
      , annotationType
      , applicationArguments
      , applicationFunction
      , functionBody
      , functionParameters
      , ifCondition
      , ifElse
      , ifThen
      , letDefinition
      , letIn
      , letName
      )
  , ParserExpression
  , ParserExpressionVariable (ParserNamedVariable)
  , ParserTopItem
  , ParserType
  , TopItem (topItemBody, topItemName)
  , Type (Arrow, BoolType, IntType, TypeVar, arrowEnd, arrowInitial)
  , makeIf
  )
import Control.Arrow ((<<<))
import Control.Monad (unless)
import Data.Map (Map)
import qualified Data.Map as Map
import Effectful (Eff, (:>))
import Effectful.Error.Dynamic (Error, throwError)
import Effectful.State.Static.Local (State, get, gets, modify)
import Effectful.Writer.Static.Local (Writer, tell)
import HistoryMap (HistoryMap, empty, lookup, popChanges, pushChanges)


data InferenceError = InferenceError deriving (Show)


newtype InferenceTypeVar = InferenceTypeVarC Int deriving (Show)


newtype InferenceExpressionVar = InferenceExpressionVarC Int
  deriving (Show, Eq, Ord)


type InferenceExpression =
  Expression InferenceExpressionVar InferenceTypeVar


type InferenceTopItem = TopItem InferenceExpressionVar InferenceTypeVar


type InferenceType = Type InferenceTypeVar


data TranslationError
  = UnboundedVariable ParserExpressionVariable
  | MultipleDefinitions [ParserExpressionVariable]
  deriving (Show)


-- TODO : raise Shadowing concerns
data TranslationWarrning = Warn deriving (Show)


data TranslationState = TranslationState
  { translationContext :: Context
  , nextExpressionVarId :: Int
  , nextTypeVarId :: Int
  }
  deriving (Show)


emptyState :: TranslationState
emptyState =
  TranslationState
    { translationContext =
        emptyContext
    , nextExpressionVarId = 0
    , nextTypeVarId = 0
    }


data Row = RowC
  { rowParserVariable :: ParserExpressionVariable
  , rowExpressionVariable :: InferenceExpressionVar
  , rowTypeVariable :: InferenceTypeVar
  , -- Parameters Doesn't have a expression dedicated to them
    rowExpression :: Maybe InferenceExpression
  }
  deriving (Show)


data Context = ContextC
  { contextByName :: HistoryMap ParserExpressionVariable Row
  , contextById :: Map InferenceExpressionVar Row
  }
  deriving (Show)


emptyContext :: Context
emptyContext =
  ContextC
    { contextByName = HistoryMap.empty
    , contextById = Map.empty
    }


{- | Checks that the list of variables doesn't
have repetitions, if it has, we throw a error.
-}
checkUniqueVariables
  :: ( Error TranslationError :> es
     , State TranslationState :> es
     )
  => [ParserExpressionVariable]
  -> Eff es ()
checkUniqueVariables pevs =
  let frequencies =
        Map.toList
          ( Map.fromListWith
              (+)
              ( (\x -> (x, 1 :: Int)) <$> pevs
              )
          )
      repeateds = filter (\x -> snd x > 1) frequencies
   in unless
        (null repeateds)
        $ throwError
          (MultipleDefinitions (fst <$> repeateds))


{- | Registers a all the variables in
| the context with a row that has empty
| the expression field
-}
registerEmpty
  :: ( Error TranslationError :> es
     , State TranslationState :> es
     )
  => [ParserExpressionVariable]
  -> Eff es [InferenceExpressionVar]
registerEmpty pevs = do
  checkUniqueVariables pevs
  rows <-
    mapM
      ( \item ->
          freshRow item >>= \x -> pure (item, x)
      )
      pevs
  context <- gets translationContext
  let newNameContext = HistoryMap.pushChanges rows (contextByName context)
  let newIdContext =
        foldr
          ( \(_, row) dic ->
              Map.insert
                (rowExpressionVariable row)
                row
                dic
          )
          (contextById context)
          rows
  modify $ \s ->
    s
      { translationContext =
          ContextC
            { contextByName =
                newNameContext
            , contextById = newIdContext
            }
      }
  pure ((rowExpressionVariable <<< snd) <$> rows)


lookupVariable
  :: ( Error TranslationError :> es
     , State TranslationState :> es
     )
  => ParserExpressionVariable
  -> Eff es Row
lookupVariable parserVar = do
  nameContext <- gets (contextByName <<< translationContext)
  case HistoryMap.lookup parserVar nameContext of
    Just row -> pure row
    Nothing -> throwError (UnboundedVariable parserVar)


freshExpressionVar
  :: State TranslationState :> es
  => Eff es InferenceExpressionVar
freshExpressionVar = do
  modify $ \s -> s {nextExpressionVarId = nextExpressionVarId s + 1}
  InferenceExpressionVarC <$> gets nextExpressionVarId


freshTypeVar
  :: State TranslationState :> es
  => Eff es InferenceTypeVar
freshTypeVar = do
  modify $ \s -> s {nextTypeVarId = nextTypeVarId s + 1}
  InferenceTypeVarC <$> gets nextTypeVarId


freshRow
  :: State TranslationState :> es
  => ParserExpressionVariable
  -> Eff es Row
freshRow pev = do
  newEVar <- freshExpressionVar
  newTVar <- freshTypeVar
  pure $
    RowC
      { rowParserVariable = pev
      , rowExpressionVariable = newEVar
      , rowTypeVariable = newTVar
      , rowExpression = Nothing
      }


cleanupParameters
  :: ( Error TranslationError :> es
     , State TranslationState :> es
     )
  => Eff es ()
cleanupParameters = modify $ \s ->
  let context = translationContext s
      byName = contextByName context
      newByName = HistoryMap.popChanges byName
   in s {translationContext = context {contextByName = newByName}}


cleanupLetNames
  :: ( Error TranslationError :> es
     , State TranslationState :> es
     )
  => [(ParserExpressionVariable, InferenceExpression)]
  -> Eff es ()
cleanupLetNames defs = do
  mapM_ (uncurry addExpressionToRow) defs
  cleanupParameters


transformType
  :: ( Error TranslationError :> es
     , Writer [TranslationWarrning] :> es
     , State TranslationState :> es
     )
  => ParserType
  -> Eff es InferenceType
transformType t =
  case t of
    IntType -> pure IntType
    BoolType -> pure BoolType
    Arrow {arrowInitial, arrowEnd} -> do
      ini <- transformType arrowInitial
      end <- mapM transformType arrowEnd
      pure Arrow {arrowInitial = ini, arrowEnd = end}
    TypeVar _ -> (pure <<< TypeVar <<< InferenceTypeVarC) (-1)


transform
  :: ( Error TranslationError :> es
     , Writer [TranslationWarrning] :> es
     , State TranslationState :> es
     )
  => ParserExpression
  -> Eff es InferenceExpression
transform expr =
  case expr of
    LiteralExpression l -> pure $ LiteralExpression l
    Variable v -> do
      value <- lookupVariable v
      pure $ Variable (rowExpressionVariable value)
    Function {functionParameters = _parameters, functionBody = _body} -> do
      newParameters <- registerEmpty _parameters
      newBody <- transform _body
      cleanupParameters
      pure
        Function {functionParameters = newParameters, functionBody = newBody}
    Application
      { applicationFunction = appFunction
      , applicationArguments =
        appArguments
      } -> do
        newFunction <- transform appFunction
        newArguments <- mapM transform appArguments
        pure
          Application
            { applicationFunction = newFunction
            , applicationArguments = newArguments
            }
    If {ifCondition = condition, ifThen = _then, ifElse = _else} -> do
      newIfCondition <- transform condition
      newIfThen <- transform _then
      newIfElse <- transform _else
      pure $ makeIf newIfCondition newIfThen newIfElse
    Let {letName = _letName, letDefinition = _letDefinition, letIn = _letIn} -> do
      newNames <- registerEmpty [_letName]
      newDefinition <- transform _letDefinition
      newIn <- transform _letIn
      cleanupLetNames
        [
          ( _letName
          , newDefinition
          )
        ]
      pure
        Let
          { letName = head newNames
          , letDefinition = newDefinition
          , letIn = newIn
          }
    Annotation {annotationExpression = ae, annotationType = at} -> do
      newExpression <- transform ae
      newType <- transformType at
      pure
        Annotation
          { annotationExpression =
              newExpression
          , annotationType = newType
          }


{- | It updates the entry of a variable in
| the context. Raises an error if
| unbound
| It doesn't update the scope context (contextByName).
-}
addExpressionToRow
  :: ( Error TranslationError :> es
     , State TranslationState :> es
     )
  => ParserExpressionVariable
  -> InferenceExpression
  -> Eff es ()
addExpressionToRow pev ie = do
  row <- lookupVariable pev
  context <- gets translationContext
  let updatedRow = row {rowExpression = Just ie}
  let updatedMap =
        Map.insert
          (rowExpressionVariable row)
          updatedRow
          (contextById context)
  modify $ \s ->
    s
      { translationContext =
          context {contextById = updatedMap}
      }


buildInferenceContextForItem
  :: ( Error TranslationError :> es
     , Writer [TranslationWarrning] :> es
     , State TranslationState :> es
     )
  => ParserTopItem
  -> Eff es ()
buildInferenceContextForItem item = do
  let name = ParserNamedVariable $ topItemName item
  let body = topItemBody item
  newBody <- transform body
  addExpressionToRow name newBody


buildInferenceContext
  :: ( Error TranslationError :> es
     , Writer
        [TranslationWarrning]
        :> es
     , State TranslationState :> es
     )
  => [ParserTopItem]
  -> Eff es (Map InferenceExpressionVar Row)
buildInferenceContext items = do
  tell [Warn]
  _ <-
    registerEmpty $
      (ParserNamedVariable <<< topItemName) <$> items
  mapM_ buildInferenceContextForItem items
  (contextById <<< translationContext) <$> get

-- infer ::
--   InferenceContext ->
--   InferenceExpression ->
--   Either InferenceError InferenceType
-- infer = error "infer is unimplemented yet!"
--
-- check ::
--   InferenceContext ->
--   InferenceExpression ->
--   InferenceType ->
--   Either InferenceError ()

-- check = error "check is unimplemented yet!"
