module Octizys.Inference.Inference2 where

import qualified Octizys.Cst.Expression as CstE
import qualified Octizys.Cst.Type as CstT

import Control.Arrow ((<<<))
import Control.Monad (foldM)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map, lookup)
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.State.Static.Local (State, gets, modify)
import qualified Octizys.Ast.Expression as AstE
import qualified Octizys.Ast.Type as AstT
import Octizys.Cst.Expression (ExpressionVariableId)
import Octizys.Cst.Type
  ( Type
  , TypeVariableId
  )
import qualified Octizys.Cst.Type as Cst
import Octizys.Cst.VariableId (VariableId (VariableId'))
import Octizys.Effects.SymbolResolution.Interpreter
  ( SourceExpressionVariableInfo (typeId)
  )
import Prettyprinter (Pretty (pretty), (<+>))
import Prelude hiding (lookup)


data InferenceError
  = -- This shouldn't happen, is a bug.
    FunctionWithoutParams CstE.Expression
  | -- This is a bug in the translation process
    UnboundExpressionVar ExpressionVariableId
  | -- This is a bug in the translation process or
    -- in the inference
    UnboundTypeVar TypeVariableId
  | CantUnify AstT.Type AstT.Type
  deriving (Show)


data Constraint = EqConstraint AstT.Type AstT.Type
  deriving (Show, Ord, Eq)


instance Pretty Constraint where
  pretty (EqConstraint t1 t2) =
    pretty t1 <+> "~" <+> pretty t2


data Output = Output'
  { constraints :: [Constraint]
  , expression :: AstE.Expression
  }
  deriving (Show, Ord, Eq)


addConstraint :: Constraint -> Output -> Output
addConstraint c o =
  o {constraints = c : o.constraints}


type ExpressionVarToInfo =
  Map ExpressionVariableId SourceExpressionVariableInfo


type TypeVarToType =
  Map TypeVariableId AstT.Type


data MapVarToConstraints = MapVarToConstraints'
  { sourceConstraints :: Map TypeVariableId Constraint
  -- ^ Constraints applied to a type variable that is the type
  -- of some expression in the AST.
  , metaConstraints :: Map TypeVariableId Constraint
  -- ^ Constraints for the meta variables.
  , nextTypeVar :: Int
  -- ^ To generate fresh type variables.
  }
  deriving (Show, Ord, Eq)


initialConstraintMap :: MapVarToConstraints
initialConstraintMap =
  MapVarToConstraints'
    { sourceConstraints = mempty
    , metaConstraints = mempty
    , nextTypeVar = 0
    }


freshTypeVar
  :: State InferenceState :> es
  => Eff es TypeVariableId
freshTypeVar = do
  next <- gets (nextTypeVar <<< constraintMap)
  modify
    ( \s ->
        s
          { constraintMap =
              s.constraintMap {nextTypeVar = next + 1}
          }
    )
  pure
    ( Cst.TypeVariableId'
        (VariableId' next)
    )


data InferenceState = InferenceState'
  { expVarTable :: ExpressionVarToInfo
  , constraintMap :: MapVarToConstraints
  , realVariablesMax :: Int
  -- ^ The counter give to us by the previous
  -- process, we know all the type variables made
  -- from users are below this number.
  }
  deriving (Show, Ord, Eq)


lookupExpressionVar
  :: ( Error InferenceError :> es
     , State InferenceState :> es
     )
  => ExpressionVariableId
  -> Eff es TypeVariableId
lookupExpressionVar var = do
  assocMap <- gets expVarTable
  case lookup var assocMap of
    Just row -> pure $ typeId row
    Nothing -> throwError (UnboundExpressionVar var)


initialInferenceState :: InferenceState
initialInferenceState =
  InferenceState'
    { expVarTable = mempty
    , constraintMap = initialConstraintMap
    , realVariablesMax = 0
    }


cstToAstType
  :: Type
  -> AstT.Type
cstToAstType t =
  case t of
    Cst.BoolType {} -> AstT.BoolType
    Cst.IntType {} -> AstT.IntType
    Cst.Arrow {start, remain} ->
      let newStart = cstToAstType start
          newRemain = cstToAstType <$> (snd <$> remain)
       in AstT.Arrow
            { start = newStart
            , remain = newRemain
            }
    Cst.Parens {_type} -> cstToAstType _type
    Cst.Variable {variableId} -> AstT.Variable {variableId}


definitionParametersToParameters
  :: ( Error InferenceError :> es
     , State InferenceState :> es
     )
  => CstE.Parameters
  -> Eff
      es
      ( NonEmpty
          ( [Constraint]
          , ExpressionVariableId
          , AstT.Type
          )
      )
definitionParametersToParameters params =
  mapM transParam (fst <$> CstE.unParameters params)
  where
    transParam p = do
      pType <- lookupExpressionVar (snd p.name)
      case p of
        CstE.ParameterAlone {} ->
          pure ([], snd p.name, AstT.Variable pType)
        CstE.ParameterWithType {} -> do
          let annotationType = cstToAstType p._type
              tableType = AstT.Variable pType
              contraintType =
                EqConstraint annotationType tableType
          pure ([contraintType], snd p.name, tableType)


definitionCstToAst
  :: ( Error InferenceError :> es
     , State InferenceState :> es
     )
  => CstE.Definition
  -> Eff es ([Constraint], AstE.Definition)
definitionCstToAst d = do
  outBody <- infer d.definition
  let newBody = outBody.expression
      newBodyType = AstE.getType newBody
      -- In case the definition has a type annotation:
      -- let f :: Annotation = ..
      annotationConstratint =
        case d.outputType of
          Nothing -> []
          Just expectedType ->
            let newExpectedType = cstToAstType expectedType
             in [EqConstraint newBodyType newExpectedType]
  case d.parameters of
    Nothing ->
      let newExp =
            AstE.Definition'
              { name =
                  snd d.name
              , inferType = newBodyType
              , definition = newBody
              }
       in pure
            ( annotationConstratint <> outBody.constraints
            , newExp
            )
    Just oldParams -> do
      newParams <- definitionParametersToParameters oldParams
      let
        constraints =
          annotationConstratint
            <> outBody.constraints
            <> concat
              ( NonEmpty.toList
                  ((\(x, _, _) -> x) <$> newParams)
              )
        newType =
          case newParams of
            ((_, _, _type) :| []) ->
              AstT.Arrow
                { start = _type
                , remain = newBodyType :| []
                }
            ((_, _, _type) :| params) ->
              AstT.Arrow
                { start = _type
                , remain =
                    NonEmpty.fromList
                      ( ( (\(_, _, _type) -> _type)
                            <$> params
                        )
                          <> [newBodyType]
                      )
                }
        newExp =
          AstE.Definition'
            { name =
                snd d.name
            , inferType = newType
            , definition =
                AstE.Function
                  { parameters = (\(_, x, y) -> (x, y)) <$> newParams
                  , body = newBody
                  , inferType = newType
                  }
            }
       in
        pure (constraints, newExp)


infer
  :: State InferenceState :> es
  => Error InferenceError :> es
  => CstE.Expression
  -> Eff es Output
infer expr =
  case expr of
    CstE.EInt {intValue} ->
      pure $
        Output'
          { constraints = []
          , expression =
              AstE.EInt
                { intValue = intValue
                , inferType = AstT.IntType
                }
          }
    CstE.EBool {boolValue} ->
      pure $
        Output'
          { constraints = []
          , expression =
              AstE.EBool
                { boolValue = boolValue
                , inferType = AstT.BoolType
                }
          }
    CstE.Variable {name} -> do
      eid <- lookupExpressionVar name
      pure $
        Output'
          { constraints = []
          , expression =
              AstE.Variable
                { name = name
                , inferType = AstT.Variable eid
                }
          }
    CstE.Parens {expression} -> infer expression
    CstE.EFunction {functionValue = f} -> do
      lastArgType <- infer f.body
      newParams <-
        mapM
          ( \x ->
              lookupExpressionVar x
                >>= \y -> pure (x, AstT.Variable y)
          )
          ( (\x -> snd x.parameter.name)
              <$> f.parameters
          )
      case NonEmpty.uncons newParams of
        (value, remain) ->
          let newEnd =
                NonEmpty.prependList
                  (maybe [] ((snd <$>) <<< NonEmpty.toList) remain)
                  ( AstE.getType lastArgType.expression
                      :| []
                  )
              inferredType = AstT.Arrow {start = snd value, remain = newEnd}
              outExpression =
                AstE.Function
                  { parameters = newParams
                  , body = lastArgType.expression
                  , inferType = inferredType
                  }
           in pure $
                Output'
                  { constraints = lastArgType.constraints
                  , expression = outExpression
                  }
    CstE.Application
      { applicationFunction = fun
      , applicationRemain = args
      } -> do
        initDom <- AstT.Variable <$> freshTypeVar
        initCodom <- AstT.Variable <$> freshTypeVar
        domOut <-
          check
            fun
            AstT.Arrow
              { start = initDom
              , remain = initCodom :| []
              }
        foldM
          ( \preOut arg -> do
              domain <- AstT.Variable <$> freshTypeVar
              codomain <- AstT.Variable <$> freshTypeVar
              let
                preIsArrow =
                  EqConstraint
                    (AstE.getType preOut.expression)
                    ( AstT.Arrow
                        { start = domain
                        , remain = codomain :| []
                        }
                    )
              outArgDomain <- check arg domain
              let
                newApp =
                  AstE.Application
                    { applicationFunction = preOut.expression
                    , applicationArgument = outArgDomain.expression
                    , inferType = AstE.getType outArgDomain.expression
                    }
              pure $
                Output'
                  { constraints =
                      preIsArrow : (outArgDomain.constraints <> preOut.constraints)
                  , expression = newApp
                  }
          )
          domOut
          args
    CstE.If {condition = cond, ifTrue = _then, ifFalse = _else} -> do
      condOut <- check cond AstT.BoolType
      thenOut <- infer _then
      elseOut <- infer _else
      -- TODO: get a error message about why the inference
      -- was needed
      let thenIsElse =
            EqConstraint
              (AstE.getType thenOut.expression)
              (AstE.getType elseOut.expression)
          newIf =
            AstE.If
              { condition = condOut.expression
              , ifTrue = thenOut.expression
              , ifFalse = elseOut.expression
              , inferType = AstE.getType thenOut.expression
              }
      pure $
        Output'
          { constraints =
              thenIsElse
                : ( elseOut.constraints
                      <> thenOut.constraints
                      <> condOut.constraints
                  )
          , expression = newIf
          }
    CstE.Let {definitions, expression = _in} -> do
      defs <- mapM definitionCstToAst (fst <$> definitions)
      inOut <- infer _in
      let
        newConstraints =
          inOut.constraints
            <> concatMap fst (NonEmpty.toList defs)
        newExp =
          AstE.Let
            { definitions = snd <$> defs
            , expression = inOut.expression
            , inferType = AstE.getType inOut.expression
            }
       in
        pure $
          Output'
            { constraints = newConstraints
            , expression = newExp
            }
    CstE.Annotation {expression = exprr, _type = ann} -> do
      let newTypeAnn = cstToAstType ann
      check exprr newTypeAnn


check
  :: State InferenceState :> es
  => Error InferenceError :> es
  => CstE.Expression
  -> AstT.Type
  -> Eff es Output
check expr ty =
  case (expr, ty) of
    (CstE.EInt {intValue}, AstT.IntType) ->
      pure $
        Output'
          { constraints = []
          , expression =
              AstE.EInt
                { intValue =
                    intValue
                , inferType = AstT.IntType
                }
          }
    (CstE.EBool {boolValue}, AstT.BoolType) ->
      pure $
        Output'
          { constraints = []
          , expression =
              AstE.EBool
                { boolValue =
                    boolValue
                , inferType = AstT.BoolType
                }
          }
    (_, _) -> do
      infered <- infer expr
      let
        inferedType =
          AstE.getType infered.expression
        newConstraint =
          EqConstraint inferedType ty
      pure $ addConstraint newConstraint infered


-- | Replaces a type variable inside a expression with another type.
subs
  :: TypeVariableId
  -- ^ The `Type` variable to be replaced.
  -> AstT.Type
  -- ^ The new value.
  -> AstT.Type
  -- ^ The value in which we substitute the variable.
  -> AstT.Type
subs vid newType oldType =
  case oldType of
    AstT.BoolType -> oldType
    AstT.IntType -> oldType
    AstT.Arrow {start, remain} ->
      AstT.Arrow
        { start = subs vid newType start
        , remain = subs vid newType <$> remain
        }
    AstT.Variable {variableId = oldVid} ->
      if oldVid == vid
        then newType
        else oldType
