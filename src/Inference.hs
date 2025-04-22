{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Inference
  ( InferenceError
  , InferenceExpressionVar (InferenceExpressionVarC)
  , InferenceTypeVar (InferenceTypeVarC)
  , InferenceExpression
  , InferenceType
  , buildInferenceContext
  , transformType
  , transform
  , InferenceTopItem
  , Context (ContextC)
  , Row (RowC)
  , freshExpressionVar
  , freshTypeVar
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
      , letName
      )
  , ParserExpression
  , ParserExpressionVariable
  , ParserTopItem
  , ParserType
  , Symbol
  , TopItem (topItemBody, topItemName)
  , Type
  , makeIf
  )
import Control.Arrow ((<<<))
import Data.Map (Map)
import qualified Data.Map as Map
import Effectful (Eff, (:>))
import Effectful.Error.Dynamic (Error, throwError)
import Effectful.State.Dynamic (State, gets, modify)
import Effectful.Writer.Dynamic (Writer)


data InferenceError = InferenceError deriving (Show)


newtype InferenceTypeVar = InferenceTypeVarC Int deriving (Show)


newtype InferenceExpressionVar = InferenceExpressionVarC Int deriving (Show)


type InferenceExpression =
  Expression InferenceExpressionVar InferenceTypeVar


type InferenceTopItem = TopItem InferenceExpressionVar InferenceTypeVar


type InferenceType = Type InferenceTypeVar


-- type InferenceContext = Context InferenceExpressionVar InferenceTypeVar

newtype TranslationError
  = UnboundedVariable ParserExpressionVariable
  deriving (Show)


data TranslationWarrning = Warn deriving (Show)


data TranslationState = TranslationState
  { translationContext :: Context
  , nextExpressionVarId :: Int
  , nextTypeVarId :: Int
  }
  deriving (Show)


data Row = RowC
  { rowParserVariable :: ParserExpressionVariable
  , rowExpressionVariable :: InferenceExpressionVar
  , rowTypeVariable :: InferenceTypeVar
  , -- Parameters Doesn't have a expression dedicated to them
    rowExpression :: Maybe InferenceExpression
  }
  deriving (Show)


data Context = ContextC
  { contextByName :: Map ParserExpressionVariable Row
  , contextById :: Map InferenceExpressionVar Row
  }
  deriving (Show)


getContext :: TranslationState -> Context
getContext = translationContext


lookupVariable
  :: ( Error TranslationError :> es
     , State TranslationState :> es
     )
  => ParserExpressionVariable
  -> Eff es Row
lookupVariable parserVar = do
  nameContext <- gets (contextByName <<< translationContext)
  case Map.lookup parserVar nameContext of
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


registerParameters
  :: ( Error TranslationError :> es
     , Writer TranslationWarrning :> es
     , State TranslationState :> es
     )
  => [ParserExpressionVariable]
  -> Eff es ()
registerParameters = undefined


cleanupParameters
  :: ( Error TranslationError :> es
     , Writer TranslationWarrning :> es
     , State TranslationState :> es
     )
  => [ParserExpressionVariable]
  -> Eff es [InferenceExpressionVar]
cleanupParameters = undefined


registerLetNames
  :: [ParserExpressionVariable]
  -> Eff es ()
registerLetNames = undefined


cleanupLetNames
  :: [ParserExpressionVariable]
  -> Eff es [InferenceExpressionVar]
cleanupLetNames = undefined


transformType
  :: ( Error TranslationError :> es
     , Writer TranslationWarrning :> es
     , State TranslationState :> es
     )
  => ParserType
  -> Eff es InferenceType
transformType = undefined


transform
  :: ( Error TranslationError :> es
     , Writer TranslationWarrning :> es
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
      registerParameters _parameters
      newBody <- transform _body
      newParameters <-
        cleanupParameters
          _parameters
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
    Let {letName = _letName, letDefinition = _letDefinition} -> do
      registerLetNames [_letName]
      newDefinition <- transform _letDefinition
      newNames <- cleanupLetNames [_letName]
      pure Let {letName = head newNames, letDefinition = newDefinition}
    Annotation {annotationExpression = ae, annotationType = at} -> do
      newExpression <- transform ae
      newType <- transformType at
      pure
        Annotation
          { annotationExpression =
              newExpression
          , annotationType = newType
          }


registerExpression
  :: ( Error TranslationError :> es
     , Writer TranslationWarrning :> es
     , State TranslationState :> es
     )
  => Symbol
  -> InferenceExpression
  -> Eff es ()
registerExpression = undefined


buildInferenceContextForItem
  :: ( Error TranslationError :> es
     , Writer TranslationWarrning :> es
     , State TranslationState :> es
     )
  => ParserTopItem
  -> Eff es ()
buildInferenceContextForItem item = do
  let name = topItemName item
  let body = topItemBody item
  newBody <- transform body
  registerExpression name newBody


registerDefinition
  :: ( Error TranslationError :> es
     , Writer TranslationWarrning :> es
     , State TranslationState :> es
     )
  => ParserTopItem
  -> Eff es ()
registerDefinition = undefined


buildInferenceContext
  :: ( Error TranslationError :> es
     , Writer TranslationWarrning :> es
     , State TranslationState :> es
     )
  => [ParserTopItem]
  -> Eff es Context
buildInferenceContext ls = do
  mapM_ registerDefinition ls
  mapM_ buildInferenceContextForItem ls
  gets getContext

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
