{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Octizys.Inference.Inference where

import Control.Monad (foldM)
import Data.Map (Map)
import qualified Data.Map as Map
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.State.Static.Local (State, gets)
import Octizys.Ast
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
      , letDefinitions
      , letIn
      )
  , LetDefinition (letDefinition, letName)
  , Literal (BoolLiteral, IntLiteral)
  , Type (Arrow, BoolType, IntType, arrowEnd, arrowInitial)
  )
import Octizys.Inference.Translation
  ( InferenceExpression
  , InferenceExpressionVar
  , InferenceType
  , InferenceTypeVar
  , Row (rowTypeVariable)
  , TranslationState
  )
import qualified Octizys.Inference.Translation as Translation


data InferenceError
  = -- This shouldn't happen, is a bug.
    FunctionWithoutParams InferenceExpression
  | -- This is a bug in the translation process
    UnboundExpressionVar InferenceExpressionVar
  | -- This is a bug in the translation process or
    -- in the inference
    UnboundTypeVar InferenceTypeVar
  | CantUnify InferenceType InferenceType
  deriving (Show)


data InferenceState = InferenceStateC
  { associationMap :: Map InferenceExpressionVar Translation.Row
  -- ^ Contains the map from Expression variables to
  -- information about them like the type variable associate.
  , associatedVariablesMap :: Map InferenceTypeVar InferenceType
  -- ^ Only contains variables that happens in `associationMap`
  -- | all of them where made with User or Real constructors.
  , metaVariablesMap :: Map Int (Maybe InferenceType)
  -- ^ All meta variables generated in the inference.
  -- A Nothing means that the variable is new without constraints.
  , nextTypeVar :: Int
  }


addTranslation
  :: TranslationState
  -> InferenceState
  -> InferenceState
addTranslation = undefined


-- Update the

inferLiteral :: Literal -> InferenceType
inferLiteral (IntLiteral _) = IntType
inferLiteral (BoolLiteral _) = BoolType


-- | Looks on the associationMap, raise Error if not found
lookupExpressionVar
  :: ( Error InferenceError :> es
     , State InferenceState :> es
     )
  => InferenceExpressionVar
  -> Eff es InferenceType
lookupExpressionVar var = do
  assocMap <- gets associationMap
  case Map.lookup var assocMap of
    Just row -> do
      let typeVar = rowTypeVariable row
      typeVarsToTypes <- gets associatedVariablesMap
      case Map.lookup typeVar typeVarsToTypes of
        Just value -> pure value
        Nothing -> throwError (UnboundTypeVar typeVar)
    Nothing -> throwError (UnboundExpressionVar var)


unify
  :: ( Error InferenceError :> es
     , State InferenceState :> es
     )
  => InferenceType
  -> InferenceType
  -> Eff es ()
unify = undefined


{- | Update (by unification) the type in the type var associated
with the variable
-}
updateExpressionVars
  :: ( Error InferenceError :> es
     , State InferenceState :> es
     )
  => [(InferenceExpressionVar, InferenceType)]
  -> Eff es ()
updateExpressionVars = undefined


-- | Generate a new fresh meta variable
freshMetaVar :: Eff es InferenceType
freshMetaVar = undefined


infer
  :: ( Error InferenceError :> es
     , State InferenceState :> es
     )
  => InferenceExpression
  -> Eff es InferenceType
infer e =
  case e of
    LiteralExpression l -> pure $ inferLiteral l
    Variable v -> lookupExpressionVar v
    Function {functionParameters = params, functionBody = body} -> do
      lastArgType <- infer body
      newParams <- mapM lookupExpressionVar params
      case newParams of
        [] -> throwError (FunctionWithoutParams body)
        (value : remain) ->
          let newEnd = remain <> [lastArgType]
           in pure Arrow {arrowInitial = value, arrowEnd = newEnd}
    Application
      { applicationFunction = fun
      , applicationArguments = args
      } -> do
        initialVar <- freshMetaVar
        foldM
          ( \t arg -> do
              domain <- freshMetaVar
              codomain <- freshMetaVar
              unify t Arrow {arrowInitial = domain, arrowEnd = [codomain]}
              _ <- check arg codomain
              pure codomain
          )
          initialVar
          (fun : args)
    If {ifCondition = cond, ifThen = _then, ifElse = _else} -> do
      _ <- check cond BoolType
      t1 <- infer _then
      t2 <- infer _else
      -- TODO: get a error message about why the inference
      -- was needed
      unify t1 t2
      pure t1
    Let {letDefinitions = definitions, letIn = _in} -> do
      let splitVars = (\d -> (letName d, letDefinition d)) <$> definitions
      types <-
        mapM
          ( \(x, y) -> do
              yResult <- infer y
              pure (x, yResult)
          )
          splitVars
      updateExpressionVars types
      -- Thanks to the translation process all variables
      -- are unique, as so, we don't need to create scopes
      -- anymore and can evaluate this as is.
      infer _in
    Annotation {annotationExpression = expr, annotationType = ann} ->
      check expr ann


check
  :: ( Error InferenceError :> es
     , State InferenceState :> es
     )
  => InferenceExpression
  -> InferenceType
  -> Eff es InferenceType
check e t = do
  inferred <- infer e
  unify t inferred
  pure inferred
