{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Octizys.Inference.Inference where

import qualified Octizys.Cst.Expression as CstE
import qualified Octizys.Cst.Type as CstT

import Control.Arrow ((<<<))
import Control.Monad (foldM)
import qualified Data.Bifunctor
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map, lookup)
import Data.Text (Text)
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
import Octizys.Effects.Logger.Effect (Logger, debug)
import Octizys.Effects.SymbolResolution.Interpreter
  ( SourceExpressionVariableInfo (typeId)
  )
import Octizys.Pretty.Expression (prettyExpression)
import Prettyprinter (Doc, Pretty (pretty), (<+>))
import qualified Prettyprinter as Pretty
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


instance Pretty Output where
  pretty Output' {constraints, expression} =
    Pretty.parens
      ( pretty constraints
          <+> ","
          <+> pretty expression
      )


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
              constraintType =
                EqConstraint annotationType tableType
          pure ([constraintType], snd p.name, tableType)


definitionCstToAst
  :: ( Error InferenceError :> es
     , State InferenceState :> es
     , Logger :> es
     )
  => CstE.Definition
  -> Eff es ([Constraint], AstE.Definition)
definitionCstToAst d = do
  outBody <- infer d.definition
  -- In
  -- let f : a, b -> c = body
  -- We need to associate the type of `f` with
  -- `a -> b-> c` and `body` with `c`
  nameTypeVar <- lookupExpressionVar (snd d.name)
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
          -- `f ~ body`
          fullDefinitionConstraint =
            EqConstraint (AstT.Variable nameTypeVar) newBodyType
       in pure
            ( fullDefinitionConstraint
                : annotationConstratint <> outBody.constraints
            , newExp
            )
    Just oldParams -> do
      newParams <- definitionParametersToParameters oldParams
      let
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
        -- `f ~ a -> b -> c`
        fullDefinitionConstraint =
          EqConstraint (AstT.Variable nameTypeVar) newType
        constraints =
          fullDefinitionConstraint
            : annotationConstratint
              <> outBody.constraints
              <> concat
                ( NonEmpty.toList
                    ((\(x, _, _) -> x) <$> newParams)
                )
       in
        pure (constraints, newExp)


inferLog
  :: Logger :> es
  => Doc ann
  -> Eff es ()
inferLog d = debug (pretty @Text "Infer:" <> d)


infer
  :: State InferenceState :> es
  => Error InferenceError :> es
  => Logger :> es
  => CstE.Expression
  -> Eff es Output
infer expr = do
  inferLog
    ( pretty @Text "Got:"
        <> prettyExpression
          pretty
          pretty
          expr
    )
  out <-
    case expr of
      CstE.EInt {intValue} -> do
        pure $
          Output'
            { constraints = []
            , expression =
                AstE.EInt
                  { intValue = intValue
                  , inferType = AstT.IntType
                  }
            }
      CstE.EBool {boolValue} -> do
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
          initialOut <- infer fun
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
                      , inferType = codomain
                      }
                pure $
                  Output'
                    { constraints =
                        preIsArrow : (outArgDomain.constraints <> preOut.constraints)
                    , expression = newApp
                    }
            )
            initialOut
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
  inferLog (pretty @Text "Out:" <> pretty out)
  pure out


checkLog
  :: Logger :> es
  => Doc ann
  -> Eff es ()
checkLog d = debug (pretty @Text "Check:" <> d)


check
  :: State InferenceState :> es
  => Error InferenceError :> es
  => Logger :> es
  => CstE.Expression
  -> AstT.Type
  -> Eff es Output
check expr ty = do
  checkLog
    ( pretty @Text "Got exp:"
        <> prettyExpression pretty pretty expr
    )
  checkLog (pretty @Text "Got type:" <> pretty ty)
  out <-
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
      -- TODO:
      -- ( CstE.EFunction
      --     (CstE.Function' {parameters, body})
      --   , AstT.Arrow {start, remain}
      --   ) -> do
      --     bodyOut <- infer body
      --     go
      --       bodyOut
      --       parameters
      --       (NonEmpty.cons start remain)
      --     where
      --       go bOut [] (last :| []) =
      --         pure $ EqConstraint (AstE.getType bOut.expression) last
      --       go bOut (x:[]) (last :| []) = do
      --         xId <- lookupExpressionVar
      --         newArrow = AstT.Arrow {
      --           AstT.start =
      --                               }
      (_, _) -> do
        inferred <- infer expr
        let
          inferredType =
            AstE.getType inferred.expression
          newConstraint =
            EqConstraint inferredType ty
        pure $ addConstraint newConstraint inferred
  checkLog (pretty @Text "Out:" <> pretty out)
  pure out


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


subsInConstraint
  :: TypeVariableId
  -> AstT.Type
  -> Constraint
  -> Constraint
subsInConstraint vid t (EqConstraint l r) =
  EqConstraint (subs vid t l) (subs vid t r)


data Substitution = Substitution'
  { variableId :: TypeVariableId
  , value :: AstT.Type
  }
  deriving (Show, Eq, Ord)


instance Pretty Substitution where
  pretty (Substitution' var value) =
    pretty '_'
      <> pretty var
      <+> pretty '~'
      <+> pretty value


findSubstitutions
  :: Error InferenceError :> es
  => [Constraint]
  -> Eff es [Substitution]
findSubstitutions [] = pure []
findSubstitutions (x : ys) = do
  initialSet <- go
  pure $ subsInInitial initialSet initialSet
  where
    go =
      case x of
        EqConstraint left right ->
          case left of
            AstT.Variable t -> do
              lst <-
                findSubstitutions
                  (subsInConstraint t right <$> ys)
              pure (Substitution' t right : lst)
            _ ->
              case right of
                AstT.Variable t -> do
                  lst <-
                    findSubstitutions
                      (subsInConstraint t right <$> ys)
                  pure (Substitution' t left : lst)
                _ -> do
                  unifySubs <- unify left right
                  findSubstitutions (unifySubs <> ys)
    subsSub s s2 = s2 {value = subs s.variableId s.value s2.value}
    subsInInitial [] end = end
    subsInInitial (z : zs) ss =
      subsInInitial zs (subsSub z <$> ss)


unify
  :: Error InferenceError :> es
  => AstT.Type
  -> AstT.Type
  -> Eff es [Constraint]
unify x y =
  if x == y
    then pure []
    else case (x, y) of
      (AstT.Arrow {}, AstT.Arrow {}) -> do
        startU <- unify x.start y.start
        args <- go x.remain y.remain
        pure (startU <> args)
        where
          go (lastX :| []) (lastY :| []) = unify lastX lastY
          go (headX :| (headX2 : lastX)) (lastY :| []) =
            unify
              ( AstT.Arrow
                  { AstT.start =
                      headX
                  , AstT.remain = headX2 :| lastX
                  }
              )
              lastY
          go (lastX :| []) (headY :| (headY2 : lastY)) =
            unify
              lastX
              ( AstT.Arrow
                  { AstT.start =
                      headY
                  , AstT.remain = headY2 :| lastY
                  }
              )
          go (someX :| (headX : moreX)) (someY :| (headY : moreY)) = do
            heads <- unify someX someY
            remains <- go (headX :| moreX) (headY :| moreY)
            pure (heads <> remains)
      (AstT.Variable _, _) ->
        pure [EqConstraint x y]
      (_, AstT.Variable _) ->
        pure [EqConstraint y x]
      _ -> throwError $ CantUnify x y


typeSubs :: [Substitution] -> AstT.Type -> AstT.Type
typeSubs [] t = t
typeSubs (Substitution' l r : xs) t =
  typeSubs xs $
    subs l r t


defSubs :: [Substitution] -> AstE.Definition -> AstE.Definition
defSubs [] d = d
defSubs l d@(AstE.Definition' {definition, inferType}) =
  d
    { AstE.definition = expSubs l definition
    , AstE.inferType = typeSubs l inferType
    }


expSubs :: [Substitution] -> AstE.Expression -> AstE.Expression
expSubs [] e = e
expSubs l e =
  case e of
    AstE.EInt {inferType} ->
      e {AstE.inferType = typeSubs l inferType}
    AstE.EBool {inferType} ->
      e {AstE.inferType = typeSubs l inferType}
    AstE.Variable {inferType} ->
      e {AstE.inferType = typeSubs l inferType}
    AstE.Function
      { parameters
      , body
      , inferType
      } ->
        e
          { AstE.inferType = typeSubs l inferType
          , AstE.parameters =
              Data.Bifunctor.second (typeSubs l) <$> parameters
          , AstE.body =
              expSubs l body
          }
    AstE.Application
      { applicationFunction
      , applicationArgument
      , inferType
      } ->
        e
          { AstE.inferType = typeSubs l inferType
          , AstE.applicationFunction =
              expSubs l applicationFunction
          , AstE.applicationArgument =
              expSubs l applicationArgument
          }
    AstE.If {inferType, condition, ifTrue, ifFalse} ->
      e
        { AstE.inferType = typeSubs l inferType
        , AstE.condition = expSubs l condition
        , AstE.ifTrue = expSubs l ifTrue
        , AstE.ifFalse = expSubs l ifFalse
        }
    AstE.Let {inferType, definitions, expression} ->
      e
        { AstE.inferType = typeSubs l inferType
        , AstE.definitions = defSubs l <$> definitions
        , AstE.expression = expSubs l expression
        }
    AstE.Annotation {expression, _type, inferType} ->
      e
        { AstE.inferType = typeSubs l inferType
        , AstE._type = typeSubs l _type
        , AstE.expression = expSubs l expression
        }
