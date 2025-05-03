{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Octizys.Inference.Inference where

import Control.Arrow ((<<<))
import Control.Monad (foldM)
import Data.Map (Map)
import qualified Data.Map as Map
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.State.Static.Local (State, gets, modify)
import Octizys.Cst.Expression
  ( Definition
      ( definition
      , name
      , outputType
      , parameters
      )
  , Expression
    ( Annotation
    , Application
    , EBool
    , EFunction
    , EInt
    , If
    , Let
    , Parens
    , Variable
    , applicationFunction
    , applicationRemain
    , boolValue
    , condition
    , definitions
    , expression
    , functionValue
    , ifFalse
    , ifTrue
    , intValue
    , name
    , _type
    )
  , ExpressionVariableId
  , Function (body, parameters)
  , FunctionParameter (parameter)
  , Parameter (ParameterAlone, ParameterWithType, name, _type)
  , Parameters (unParameters)
  )
import Octizys.Cst.Type
  ( Type
  , TypeVariableId
  )
import qualified Octizys.Cst.Type as Cst
import Octizys.Effects.SymbolResolution.Interpreter
  ( SourceExpressionVariableInfo (typeId)
  )

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Octizys.Ast.Expression as Ast
import qualified Octizys.Ast.Expression as AstE
import qualified Octizys.Ast.Type as Ast
import qualified Octizys.Ast.Type as AstT
import Octizys.Cst.VariableId (VariableId (VariableId'))


data InferenceError
  = -- This shouldn't happen, is a bug.
    FunctionWithoutParams Expression
  | -- This is a bug in the translation process
    UnboundExpressionVar ExpressionVariableId
  | -- This is a bug in the translation process or
    -- in the inference
    UnboundTypeVar TypeVariableId
  | CantUnify Ast.Type Ast.Type
  deriving (Show)


type ExpressionVarToInfo =
  Map ExpressionVariableId SourceExpressionVariableInfo


type TypeVarToType =
  Map TypeVariableId Ast.Type


data InferenceState = InferenceStateC
  { expVarTable :: ExpressionVarToInfo
  , typeVarToType :: TypeVarToType
  , realVariablesMax :: Int
  -- ^ The counter give to us by the previous
  -- process, we know all the variables made
  -- from users are below this number.
  , nextTypeVar :: Int
  }


-- Update the

-- | Looks on the associationMap, raise Error if not found
lookupExpressionVar
  :: ( Error InferenceError :> es
     , State InferenceState :> es
     )
  => ExpressionVariableId
  -> Eff es Ast.Type
lookupExpressionVar var = do
  assocMap <- gets expVarTable
  case Map.lookup var assocMap of
    Just row -> do
      let typeVar = typeId row
      typeVarsToTypes <- gets typeVarToType
      case Map.lookup typeVar typeVarsToTypes of
        Just value -> pure value
        Nothing -> throwError (UnboundTypeVar typeVar)
    Nothing -> throwError (UnboundExpressionVar var)


unify
  :: ( Error InferenceError :> es
     , State InferenceState :> es
     )
  => Ast.Type
  -> Ast.Type
  -> Eff es ()
unify t1 t2
  | t1 == t2 = pure ()
unify (Ast.Arrow s1 r1) (Ast.Arrow s2 r2) = do
  unify s1 s2
  unifyList (NonEmpty.toList r1) (NonEmpty.toList r2)
  where
    unifyList [] [] = pure ()
    unifyList (x : xs) (y : ys) = unify x y >> unifyList xs ys
    unifyList _ _ = throwError (CantUnify (AstT.Arrow s1 r1) (AstT.Arrow s2 r2))
unify (AstT.Variable v1) (AstT.Variable v2)
  | v1 == v2 = pure ()
unify (AstT.Variable v) t = unifyVar v t
unify t (AstT.Variable v) = unifyVar v t
unify t1 t2 = throwError (CantUnify t1 t2)


-- Helper
unifyVar
  :: ( Error InferenceError :> es
     , State InferenceState :> es
     )
  => TypeVariableId
  -> Ast.Type
  -> Eff es ()
unifyVar v t = do
  state <- gets typeVarToType
  case Map.lookup v state of
    Just existing -> unify existing t
    Nothing -> do
      let updateFn s = s {typeVarToType = Map.insert v t (typeVarToType s)}
      Effectful.State.Static.Local.modify updateFn


-- | Generate a new fresh meta variable
freshMetaVar
  :: State InferenceState :> es
  => Eff es AstT.Type
freshMetaVar = do
  next <- gets nextTypeVar
  modify (\s -> s {nextTypeVar = next + 1})
  pure
    ( AstT.Variable
        ( Cst.TypeVariableId'
            (VariableId' next)
        )
    )


definitionParametersToParameters
  :: ( Error InferenceError :> es
     , State InferenceState :> es
     )
  => Parameters
  -> Eff es (NonEmpty (ExpressionVariableId, Ast.Type))
definitionParametersToParameters params =
  mapM transParam (fst <$> unParameters params)
  where
    transParam p = do
      pType <- lookupExpressionVar (snd p.name)
      case p of
        ParameterAlone {} ->
          pure (snd p.name, pType)
        ParameterWithType {} -> do
          let newType = cstToAstType p._type
          unify newType pType
          pure (snd p.name, pType)


cstToAstType
  :: Type
  -> Ast.Type
cstToAstType t =
  case t of
    Cst.BoolType {} -> Ast.BoolType
    Cst.IntType {} -> Ast.IntType
    Cst.Arrow {start, remain} ->
      let newStart = cstToAstType start
          newRemain = cstToAstType <$> (snd <$> remain)
       in Ast.Arrow
            { start = newStart
            , remain = newRemain
            }
    Cst.Parens {_type} -> cstToAstType _type
    Cst.Variable {variableId} -> AstT.Variable {variableId}


definitionCstToAst
  :: ( Error InferenceError :> es
     , State InferenceState :> es
     )
  => Definition
  -> Eff es Ast.Definition
definitionCstToAst d = do
  newBody <- infer d.definition
  case d.outputType of
    Nothing -> pure ()
    Just expectedType -> do
      let newExpectedType = cstToAstType expectedType
      unify (snd newBody) newExpectedType
  case d.parameters of
    Nothing ->
      let newExp =
            Ast.Definition'
              { name =
                  snd d.name
              , inferType = snd newBody
              , definition = fst newBody
              }
       in pure newExp
    Just oldParams -> do
      newParams <- definitionParametersToParameters oldParams
      let
        newType =
          case newParams of
            (param :| []) ->
              Ast.Arrow
                { start = snd param
                , remain = snd newBody :| []
                }
            (param :| params) ->
              Ast.Arrow
                { start = snd param
                , remain =
                    NonEmpty.fromList
                      ( ( snd
                            <$> params
                        )
                          <> [snd newBody]
                      )
                }
        newExp =
          Ast.Definition'
            { name =
                snd d.name
            , inferType = newType
            , definition =
                Ast.Function
                  { parameters = newParams
                  , body = fst newBody
                  , inferType = newType
                  }
            }
       in
        pure newExp


infer
  :: ( Error InferenceError :> es
     , State InferenceState :> es
     )
  => Expression
  -> Eff es (Ast.Expression, Ast.Type)
infer e =
  case e of
    EInt {intValue} ->
      pure
        ( Ast.EInt
            { intValue
            , inferType = Ast.IntType
            }
        , Ast.IntType
        )
    EBool {boolValue} ->
      pure
        ( Ast.EBool
            { boolValue
            , inferType = Ast.BoolType
            }
        , Ast.BoolType
        )
    Variable {name} -> do
      t <- lookupExpressionVar name
      pure (AstE.Variable {name, inferType = t}, t)
    Parens {expression} -> infer expression
    EFunction {functionValue = f} -> do
      lastArgType <- infer f.body
      newParams <-
        mapM
          (\x -> lookupExpressionVar x >>= \y -> pure (x, y))
          ( (\x -> snd x.parameter.name)
              <$> f.parameters
          )
      case NonEmpty.uncons newParams of
        (value, remain) ->
          let newEnd =
                NonEmpty.prependList
                  (maybe [] ((snd <$>) <<< NonEmpty.toList) remain)
                  (snd lastArgType :| [])
              inferredType = Ast.Arrow {start = snd value, remain = newEnd}
           in pure
                ( Ast.Function
                    { parameters = newParams
                    , body = fst lastArgType
                    , inferType = inferredType
                    }
                , inferredType
                )
    Application
      { applicationFunction = fun
      , applicationRemain = args
      } -> do
        initDom <- freshMetaVar
        initCodom <- freshMetaVar
        (newFun, funType) <-
          check
            fun
            Ast.Arrow
              { start = initDom
              , remain = initCodom :| []
              }
        foldM
          ( \t arg -> do
              domain <- freshMetaVar
              codomain <- freshMetaVar
              unify
                (snd t)
                Ast.Arrow
                  { start = domain
                  , remain = codomain :| []
                  }
              (newArg, codom) <- check arg domain
              let
                newApp =
                  Ast.Application
                    { applicationFunction = fst t
                    , applicationArgument = newArg
                    , inferType = codom
                    }
              pure (newApp, codom)
          )
          (newFun, funType)
          args
    If {condition = cond, ifTrue = _then, ifFalse = _else} -> do
      (newCond, _) <- check cond Ast.BoolType
      (newThen, thenType) <- infer _then
      (newElse, elseType) <- infer _else
      -- TODO: get a error message about why the inference
      -- was needed
      unify thenType elseType
      let newIf =
            Ast.If
              { condition = newCond
              , ifTrue = newThen
              , ifFalse = newElse
              , inferType = thenType
              }
      pure (newIf, thenType)
    Let {definitions, expression = _in} -> do
      defs <- mapM definitionCstToAst (fst <$> definitions)
      (inExp, inType) <- infer _in
      let newExp = Ast.Let {definitions = defs, expression = inExp, inferType = inType}
       in pure (newExp, inType)
    Annotation {expression = expr, _type = ann} -> do
      let newTypeAnn = cstToAstType ann
      check expr newTypeAnn


check
  :: ( Error InferenceError :> es
     , State InferenceState :> es
     )
  => Expression
  -> Ast.Type
  -> Eff es (Ast.Expression, Ast.Type)
check e t = do
  (newExpression, newType) <- infer e
  unify t newType
  pure (newExpression, newType)
