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
import Effectful.State.Static.Local (State, get, gets, modify)
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
import Data.Set (Set)
import qualified Data.Set as Set
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


data Constraint
  = IsType Ast.Type
  | IsMetaVariable TypeVariableId
  | UnInitialized
  deriving (Show, Ord, Eq)


newtype MetaVariable = MetaVariable' {unMetaVariable :: TypeVariableId}
  deriving (Show, Ord, Eq)


data InferenceState = InferenceState'
  { expVarTable :: ExpressionVarToInfo
  , typeVarToType :: TypeVarToType
  , metaVariables :: Map MetaVariable Constraint
  , realVariablesMax :: Int
  -- ^ The counter give to us by the previous
  -- process, we know all the type variables made
  -- from users are below this number.
  , nextTypeVar :: Int
  }
  deriving (Show, Ord, Eq)


initialInferenceState :: InferenceState
initialInferenceState =
  InferenceState'
    { expVarTable = mempty
    , typeVarToType = mempty
    , metaVariables = mempty
    , realVariablesMax = 0
    , nextTypeVar = 0
    }


-- Update the

-- | Looks on the associationMap
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
        Nothing -> do
          newMeta <- freshMetaVariable
          let asType = AstT.Variable (unMetaVariable newMeta)
          modify
            ( \s ->
                s
                  { typeVarToType =
                      Map.insert typeVar asType typeVarsToTypes
                  }
            )
          pure asType
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
  st <- get
  case Map.lookup v st.typeVarToType of
    Just existing -> unify existing t
    Nothing -> do
      case Map.lookup (MetaVariable' v) st.metaVariables of
        Just existing ->
          case existing of
            UnInitialized ->
              let updateFn s =
                    s
                      { metaVariables =
                          Map.insert
                            (MetaVariable' v)
                            (IsType t)
                            s.metaVariables
                      }
               in modify updateFn
            IsType t2 -> unify t2 t
            IsMetaVariable metaId -> unifyVar metaId t
        Nothing -> throwError $ UnboundTypeVar v


-- | Generate a new fresh meta variable
freshTypeVarId
  :: State InferenceState :> es
  => Eff es TypeVariableId
freshTypeVarId = do
  next <- gets nextTypeVar
  modify (\s -> s {nextTypeVar = next + 1})
  pure
    ( Cst.TypeVariableId'
        (VariableId' next)
    )


freshMetaVariable
  :: State InferenceState :> es
  => Eff es MetaVariable
freshMetaVariable = do
  newId <- freshTypeVarId
  modify
    ( \s ->
        s
          { metaVariables =
              Map.insert (MetaVariable' newId) UnInitialized (metaVariables s)
          }
    )
  pure (MetaVariable' newId)


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


metaToVar :: MetaVariable -> Ast.Type
metaToVar = AstT.Variable <<< unMetaVariable


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
        initDom <- metaToVar <$> freshMetaVariable
        initCodom <- metaToVar <$> freshMetaVariable
        (newFun, funType) <-
          check
            fun
            Ast.Arrow
              { start = initDom
              , remain = initCodom :| []
              }
        foldM
          ( \t arg -> do
              domain <- metaToVar <$> freshMetaVariable
              codomain <- metaToVar <$> freshMetaVariable
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


findTypeBoundTypeVariables
  :: Ast.Type -> Set TypeVariableId
findTypeBoundTypeVariables t =
  case t of
    AstT.BoolType -> mempty
    AstT.IntType -> mempty
    AstT.Arrow {start, remain} ->
      foldr
        (\x acc -> Set.union acc (findTypeBoundTypeVariables x))
        (findTypeBoundTypeVariables start)
        remain
    AstT.Variable {variableId} -> Set.singleton variableId


findExpressionBoundTypeVariables
  :: Ast.Expression
  -> Set TypeVariableId
findExpressionBoundTypeVariables e =
  case e of
    Ast.EInt {} -> mempty
    Ast.EBool {} -> mempty
    AstE.Variable {inferType} ->
      findTypeBoundTypeVariables inferType
    Ast.Function {parameters, inferType} ->
      Set.union
        (findTypeBoundTypeVariables inferType)
        ( foldr
            ( (\x acc -> Set.union acc (findTypeBoundTypeVariables x))
                <<< snd
            )
            mempty
            parameters
        )
    Ast.Application {applicationFunction, applicationArgument} ->
      Set.union
        (findExpressionBoundTypeVariables applicationFunction)
        (findExpressionBoundTypeVariables applicationArgument)
    Ast.If {condition, ifTrue, ifFalse, inferType} ->
      Set.union
        ( Set.union
            ( Set.union
                (findExpressionBoundTypeVariables condition)
                (findExpressionBoundTypeVariables ifTrue)
            )
            (findExpressionBoundTypeVariables ifFalse)
        )
        (findTypeBoundTypeVariables inferType)
    Ast.Let {definitions, expression, inferType} ->
      foldr
        ( \d acc ->
            Set.union
              (Set.union acc (findExpressionBoundTypeVariables d.definition))
              (findTypeBoundTypeVariables d.inferType)
        )
        ( Set.union
            (findExpressionBoundTypeVariables expression)
            (findTypeBoundTypeVariables inferType)
        )
        definitions
    Ast.Annotation {expression, _type, inferType} ->
      Set.union
        ( Set.union
            (findExpressionBoundTypeVariables expression)
            (findTypeBoundTypeVariables _type)
        )
        (findTypeBoundTypeVariables inferType)


substituteVariableInType
  :: TypeVariableId
  -> Ast.Type
  -> Ast.Type
  -> Ast.Type
substituteVariableInType = undefined


substituteVariable
  :: TypeVariableId
  -> Expression
  -> Expression
substituteVariable = undefined

-- substituteMetaVariables
--   :: ( Error InferenceError :> es
--      , State InferenceState :> es
--      )
--   => [Ast.Expression]
--   -> Eff es [Ast.Expression]
-- substituteMetaVariables exps = do
--   let meaningfulVariables =
--         foldr
--           ( \x acc ->
--               Set.union acc (findExpressionBoundTypeVariables x)
--           )
--           exps
--   metaVars <- gets metaVariables
--   loop (sortOn Map.toList metaVars) meaningfulVariables
--   where
--     loop [] m = pure ()
--     loop (x : y) m = do
--       if Set.elem x m then
--         substituteVariable x
--       else
--       substituteVariable x y m
--       loop y m
