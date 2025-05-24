{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module Octizys.Inference.Inference where

import qualified Octizys.Cst.Expression as CstE
import qualified Octizys.Cst.Type as CstT

import Control.Arrow ((<<<))
import Control.Monad (foldM, forM, unless)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map, lookup)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.State.Static.Local (State, gets, modify)
import Octizys.Ast.Expression (freeTypeVars)
import qualified Octizys.Ast.Expression as AstE
import qualified Octizys.Ast.Type as AstT
import Octizys.Cst.Expression (ExpressionVariableId)
import Octizys.Cst.InfoId
  ( HasInfoSpan (getInfoSpan)
  , InfoId
  , InfoSpan (OneInfo, TwoInfo)
  , infoSpanEnd
  )
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
import Octizys.Report
  ( LongDescription (LongDescription', afterDescription, preDescription, source)
  )
import Prettyprinter (Doc, Pretty (pretty), (<+>))
import qualified Prettyprinter as Pretty
import Prelude hiding (lookup)


data InferenceError
  = -- This shouldn't happen, is a bug.
    FunctionWithoutParams CstE.Expression
  | -- This is a bug in the translation process
    UnboundExpressionVar ExpressionVariableId
  | CantUnify Constraint
  | ExpressionContainsFreeVariablesAfterSolving
      AstE.Expression
      CstE.Expression
      (Set.Set TypeVariableId)
      [Substitution]
  | DefinitionContainsFreeVariablesAfterSolving
      AstE.Definition
      CstE.Definition
      (Set.Set TypeVariableId)
      [Substitution]
  | RecursiveSubstitution ConstraintReason InfoSpan TypeVariableId AstT.Type
  deriving (Show)


data ConstraintReason
  = -- | The condition inside an if should be a boolean type
    IfConditionShouldBeBool
  | -- | The cases inside a if should have the same type.
    IfCasesShouldMatch
  | ApplicationShouldBeOnArrows
  | ArgumentShouldBeOfDomainType
  | TypeAnnotation
  | -- | A parameter was found with type annotation
    ParameterTypeAnnotation
  | -- | A variable was defined without arguments, we must be sure
    -- that it's body has the same type as the one assigned to the name.
    -- `let f = body` => `type f ~ type body`
    DefinitionVariableAndBodyShouldBeEqual
  | -- | f : Type
    DefinitionTypeAnnotation
  | -- | f : x , y , Type
    DefinitionTypeAnnotationWithArgs
  | -- | We already know the type of a variable, it can be
    -- because is a builtin or we are in the repl and we solved
    -- it previously.
    IsKnowType
  deriving (Show, Eq, Ord)


buildConstraintUnifyReportDescriptions
  :: Doc ann
  -> Constraint
  -> (Text, [LongDescription ann])
buildConstraintUnifyReportDescriptions locationCstDoc c =
  let
    start =
      LongDescription'
        { preDescription = Just "Attempted to unify:"
        , source =
            Just
              ( pretty @Text "  "
                  <> pretty c.constraintType1
                  <> Pretty.line
                  <> pretty @Text "with:"
                  <> Pretty.nest
                    2
                    ( Pretty.line <> pretty c.constraintType2
                    )
              )
        , afterDescription = Nothing
        }

    long =
      case c.reason of
        IfConditionShouldBeBool ->
          [ start
          , LongDescription'
              { preDescription = Just "While trying to check the type of an 'if' condition:"
              , source = Just locationCstDoc
              , afterDescription =
                  Just "A condition in a if expression must be of type Bool."
              }
          ]
        IfCasesShouldMatch ->
          [ start
          , LongDescription'
              { preDescription =
                  Just
                    "While trying to check that both branches of an 'if' expression have the same type:"
              , source = Just locationCstDoc
              , afterDescription =
                  Just
                    "The type of an 'if' expression must match the types of both alternative branches."
              }
          ]
        -- TODO: we can try to  get the offending left side!
        ApplicationShouldBeOnArrows ->
          [ start
          , LongDescription'
              { preDescription =
                  Just
                    "While trying to check a function application:"
              , source = Just locationCstDoc
              , afterDescription =
                  Just
                    "The expression being applied must have a function type."
              }
          ]
        -- TODO: we can try to get the right side!
        ArgumentShouldBeOfDomainType ->
          [ start
          , LongDescription'
              { preDescription =
                  Just
                    "While trying to check a function argument:"
              , source = Just locationCstDoc
              , afterDescription =
                  Just
                    "To apply a function to an expression the argument must have the correct type."
              }
          ]
        TypeAnnotation ->
          [ start
          , LongDescription'
              { preDescription =
                  Just
                    "While trying to check that the expression has the given type:"
              , source = Just locationCstDoc
              , afterDescription = Nothing
              }
          ]
        ParameterTypeAnnotation ->
          [ start
          , LongDescription'
              { preDescription =
                  Just
                    "While trying to check the type of the parameter:"
              , source = Just locationCstDoc
              , afterDescription =
                  Just
                    "A type annotation was provided for the parameter, but it conflicts with the type inferred from the function body."
              }
          ]
        DefinitionVariableAndBodyShouldBeEqual ->
          [ start
          , LongDescription'
              { preDescription =
                  Just
                    "While trying to check that the variable and the value have the same type:"
              , source = Just locationCstDoc
              , afterDescription =
                  Just
                    "A definition of a variable without arguments must have the same type as the value assigned to it."
              }
          ]
        DefinitionTypeAnnotation ->
          [ start
          , LongDescription'
              { preDescription =
                  Just
                    "While trying to check that the variable and the body have the same type:"
              , source = Just locationCstDoc
              , afterDescription =
                  Just
                    "The given value has a different type than the one specified in the variable's type signature."
              }
          ]
        DefinitionTypeAnnotationWithArgs ->
          [ start
          , LongDescription'
              { preDescription =
                  Just
                    "While trying to check that the variable and the body have the same type:"
              , source = Just locationCstDoc
              , afterDescription =
                  Just
                    "The given value has a different type than the one specified in the variable's type signature."
              }
          ]
        IsKnowType ->
          -- TODO: what can I put here?
          [start]
   in
    ( "TypeError>Can't unify types."
    , long
    )


data Constraint = EqConstraint
  { constraintType1 :: AstT.Type
  , constraintType2 :: AstT.Type
  , reason :: ConstraintReason
  , info :: InfoSpan
  }
  deriving (Show, Ord, Eq)


instance Pretty Constraint where
  pretty (EqConstraint t1 t2 _ _) =
    pretty t1 <+> "~" <+> pretty t2


prettyConstraintWithReason :: Constraint -> Doc ann
prettyConstraintWithReason c =
  pretty c <+> pretty (show c.reason)


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
  , knowTypes :: Map ExpressionVariableId AstT.Type
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


lookupKnowType
  :: ( Error InferenceError :> es
     , State InferenceState :> es
     )
  => ExpressionVariableId
  -> Eff es (Maybe AstT.Type)
lookupKnowType var = do
  assocMap <- gets knowTypes
  pure $ lookup var assocMap


initialInferenceState :: InferenceState
initialInferenceState =
  InferenceState'
    { expVarTable = mempty
    , knowTypes = mempty
    , constraintMap = initialConstraintMap
    , realVariablesMax = 0
    }


insertKnowType
  :: State InferenceState :> es
  => ExpressionVariableId
  -> AstT.Type
  -> Eff es ()
insertKnowType vid t = do
  modify (\s -> s {knowTypes = Map.insert vid t s.knowTypes})


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
  :: forall es
   . ( Error InferenceError :> es
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
  mapM transParam (CstE.unParameters params)
  where
    transParam
      :: (CstE.Parameter, InfoId)
      -> Eff es ([Constraint], ExpressionVariableId, AstT.Type)
    transParam (p, inf) = do
      pType <- lookupExpressionVar (snd p.name)
      case p of
        CstE.ParameterAlone {name = pName} ->
          pure ([], snd pName, AstT.Variable pType)
        CstE.ParameterWithType {name = pName} -> do
          let annotationType = cstToAstType p._type
              tableType = AstT.Variable pType
              constraintType =
                EqConstraint
                  { constraintType1 = annotationType
                  , constraintType2 = tableType
                  , reason = ParameterTypeAnnotation
                  , info = OneInfo inf
                  }
          pure ([constraintType], snd pName, tableType)


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
      annotationConstraint =
        case d.outputType of
          Nothing -> []
          Just expectedType ->
            let newExpectedType = cstToAstType expectedType
             in [ EqConstraint
                    { constraintType1 = newBodyType
                    , constraintType2 = newExpectedType
                    , reason = DefinitionTypeAnnotation
                    , info =
                        TwoInfo
                          (fst d.name)
                          ( infoSpanEnd $
                              getInfoSpan expectedType
                          )
                    }
                ]
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
            EqConstraint
              { constraintType1 = AstT.Variable nameTypeVar
              , constraintType2 = newBodyType
              , reason = DefinitionVariableAndBodyShouldBeEqual
              , info = getInfoSpan d
              }
       in pure
            ( fullDefinitionConstraint
                : annotationConstraint <> outBody.constraints
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
          EqConstraint
            { constraintType1 = AstT.Variable nameTypeVar
            , constraintType2 = newType
            , reason = DefinitionTypeAnnotationWithArgs
            , -- TODO: adjust this, in case we have outputType we
              -- should choose it.
              info = TwoInfo (fst d.name) d.equal
            }
        constraints =
          fullDefinitionConstraint
            : annotationConstraint
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
      CstE.Variable {name, info} -> do
        eid <- lookupExpressionVar name
        maybeType <- lookupKnowType name
        inferLog (pretty @Text "looked " <> pretty name)
        inferLog (pretty @Text "result " <> pretty (show maybeType))
        case maybeType of
          Just knowTy ->
            pure $
              Output'
                { constraints =
                    [ EqConstraint
                        { constraintType1 = AstT.Variable eid
                        , constraintType2 = knowTy
                        , reason = IsKnowType
                        , info = OneInfo info
                        }
                    ]
                , expression =
                    AstE.Variable
                      { name = name
                      , inferType = knowTy
                      }
                }
          Nothing -> do
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
          let initialSpan = getInfoSpan fun
          (out, _) <- foldM go (initialOut, initialSpan) args
          pure out
          where
            go (preOut, preSpan) arg = do
              domain <- AstT.Variable <$> freshTypeVar
              codomain <- AstT.Variable <$> freshTypeVar
              let
                preIsArrow =
                  EqConstraint
                    { constraintType1 = AstE.getType preOut.expression
                    , constraintType2 =
                        AstT.Arrow
                          { start = domain
                          , remain = codomain :| []
                          }
                    , reason = ApplicationShouldBeOnArrows
                    , info = preSpan <> getInfoSpan arg
                    }
              outArgDomain <- check arg domain ArgumentShouldBeOfDomainType
              let
                outInfo = getInfoSpan arg
                newApp =
                  AstE.Application
                    { applicationFunction = preOut.expression
                    , applicationArgument = outArgDomain.expression
                    , inferType = codomain
                    }
              pure
                ( Output'
                    { constraints =
                        preIsArrow : (outArgDomain.constraints <> preOut.constraints)
                    , expression = newApp
                    }
                , outInfo
                )
      CstE.If {condition = cond, ifTrue, ifFalse, _then, _else} -> do
        condOut <- check cond AstT.BoolType IfConditionShouldBeBool
        thenOut <- infer ifTrue
        elseOut <- infer ifFalse
        let thenIsElse =
              EqConstraint
                { constraintType1 = AstE.getType thenOut.expression
                , constraintType2 = AstE.getType elseOut.expression
                , reason = IfCasesShouldMatch
                , info =
                    TwoInfo
                      _then
                      ( infoSpanEnd $
                          getInfoSpan ifFalse
                      )
                }
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
        check exprr newTypeAnn TypeAnnotation
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
  -> ConstraintReason
  -> Eff es Output
check expr ty reason = do
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
            EqConstraint
              { constraintType1 = inferredType
              , constraintType2 = ty
              , reason = reason
              , info = getInfoSpan expr
              }
        pure $ addConstraint newConstraint inferred
  checkLog (pretty @Text "Out:" <> pretty out)
  pure out


-- | Replaces a type variable inside a expression with another type.
subs
  :: Error InferenceError :> es
  => ConstraintReason
  -> InfoSpan
  -> TypeVariableId
  -- ^ The `Type` variable to be replaced.
  -> AstT.Type
  -- ^ The new value.
  -> AstT.Type
  -- ^ The value in which we substitute the variable.
  -> Eff es AstT.Type
subs reason info vid newType oldType =
  let freeInType = AstT.freeVariables newType
   in if Set.member vid freeInType
        then throwError $ RecursiveSubstitution reason info vid newType
        else case oldType of
          AstT.BoolType -> pure oldType
          AstT.IntType -> pure oldType
          AstT.Arrow {start, remain} -> do
            newStart <- subs reason info vid newType start
            newRemain <- forM remain (subs reason info vid newType)
            pure $
              AstT.Arrow
                { start = newStart
                , remain = newRemain
                }
          AstT.Variable {variableId = oldVid} ->
            pure $
              if oldVid == vid
                then newType
                else oldType


subsInConstraint
  :: Error InferenceError :> es
  => ConstraintReason
  -> InfoSpan
  -> TypeVariableId
  -> AstT.Type
  -> Constraint
  -> Eff es Constraint
subsInConstraint rsn info vid t c = do
  newT1 <- subs rsn info vid t c.constraintType1
  newT2 <- subs rsn info vid t c.constraintType2
  pure
    c
      { constraintType1 = newT1
      , constraintType2 = newT2
      }


data Substitution = Substitution'
  { variableId :: TypeVariableId
  , value :: AstT.Type
  , reason :: ConstraintReason
  , info :: InfoSpan
  }
  deriving (Show, Eq, Ord)


instance Pretty Substitution where
  pretty (Substitution' var value _ _) =
    pretty '_'
      <> pretty var
      <+> pretty '~'
      <+> pretty value


findUnsolvedSubstitutions
  :: Set.Set TypeVariableId
  -> [Substitution]
  -> [Substitution]
findUnsolvedSubstitutions initial substitutions = go initial Set.empty []
  where
    go
      :: Set.Set TypeVariableId
      -> Set.Set TypeVariableId
      -> [Substitution]
      -> [Substitution]
    go current seen acc
      | Set.null (Set.difference current seen) = acc
      | otherwise =
          let newSeen = Set.union seen current
              (related, _) = partitionSubstitutions current substitutions
              newVars = Set.unions (map (AstT.freeVariables . value) related)
              newCurrent = Set.union newVars newSeen
           in go newCurrent newSeen (acc ++ related)

    partitionSubstitutions
      :: Set.Set TypeVariableId -> [Substitution] -> ([Substitution], [Substitution])
    partitionSubstitutions vars = foldr step ([], [])
      where
        step s (yes, no)
          | variableId s `Set.member` vars
              || not (Set.null (Set.intersection (AstT.freeVariables (value s)) vars)) =
              (s : yes, no)
          | otherwise = (yes, s : no)


findSubstitutions
  :: Error InferenceError :> es
  => [Constraint]
  -> Eff es [Substitution]
findSubstitutions [] = pure []
findSubstitutions (x : ys) = do
  initialSet <- go
  subsInInitial initialSet initialSet
  where
    go =
      case x.constraintType1 of
        AstT.Variable t -> do
          lst <-
            forM ys (subsInConstraint x.reason x.info t x.constraintType2)
              >>= findSubstitutions
          pure
            ( Substitution' t x.constraintType2 x.reason x.info
                : lst
            )
        _ ->
          case x.constraintType2 of
            AstT.Variable t -> do
              lst <-
                forM ys (subsInConstraint x.reason x.info t x.constraintType1)
                  >>= findSubstitutions
              pure
                ( Substitution' t x.constraintType1 x.reason x.info
                    : lst
                )
            _ -> do
              unifySubs <- unify x
              findSubstitutions (unifySubs <> ys)
    subsSub s s2 = do
      newVal <- subs s.reason s.info s.variableId s.value s2.value
      pure s2 {value = newVal}
    subsInInitial [] end = pure end
    subsInInitial (z : zs) ss =
      forM ss (subsSub z) >>= subsInInitial zs


unify
  :: Error InferenceError :> es
  => Constraint
  -> Eff es [Constraint]
unify c =
  let
    x = c.constraintType1
    y = c.constraintType2
   in
    if x == y
      then pure []
      else case (x, y) of
        (AstT.Arrow {remain = remainX}, AstT.Arrow {remain = remainY}) -> do
          startU <- unifyWith x.start y.start
          args <- go remainX remainY
          pure (startU <> args)
          where
            go (lastX :| []) (lastY :| []) = unifyWith lastX lastY
            go (headX :| (headX2 : lastX)) (lastY :| []) =
              unifyWith
                ( AstT.Arrow
                    { AstT.start =
                        headX
                    , AstT.remain = headX2 :| lastX
                    }
                )
                lastY
            go (lastX :| []) (headY :| (headY2 : lastY)) =
              unifyWith
                lastX
                ( AstT.Arrow
                    { AstT.start =
                        headY
                    , AstT.remain = headY2 :| lastY
                    }
                )
            go (someX :| (headX : moreX)) (someY :| (headY : moreY)) = do
              heads <- unifyWith someX someY
              remains <- go (headX :| moreX) (headY :| moreY)
              pure (heads <> remains)
        (AstT.Variable _, _) -> do
          pure [c]
        (_, AstT.Variable _) -> unifyWith y x
        _ -> throwError $ CantUnify c
  where
    unifyWith z w = unify c {constraintType1 = z, constraintType2 = w}


typeSubs
  :: Error InferenceError :> es
  => [Substitution]
  -> AstT.Type
  -> Eff es AstT.Type
typeSubs listOfSubstitutions t =
  foldM
    (\ty (Substitution' l r rsn info) -> subs rsn info l r ty)
    t
    listOfSubstitutions


defSubs
  :: Error InferenceError :> es
  => [Substitution]
  -> AstE.Definition
  -> Eff es AstE.Definition
defSubs [] d = pure d
defSubs l d@(AstE.Definition' {definition, inferType}) = do
  newDef <- expSubs l definition
  newType <- typeSubs l inferType
  pure
    d
      { AstE.definition = newDef
      , AstE.inferType = newType
      }


expSubs
  :: Error InferenceError :> es
  => [Substitution]
  -> AstE.Expression
  -> Eff es AstE.Expression
expSubs [] e = pure e
expSubs l e =
  case e of
    AstE.EInt {inferType} -> do
      newType <- typeSubs l inferType
      pure $ e {AstE.inferType = newType}
    AstE.EBool {inferType} -> do
      newType <- typeSubs l inferType
      pure $ e {AstE.inferType = newType}
    AstE.Variable {inferType} -> do
      newType <- typeSubs l inferType
      pure $ e {AstE.inferType = newType}
    AstE.Function
      { parameters
      , body
      , inferType
      } -> do
        newType <- typeSubs l inferType
        newParams <- forM parameters (\(x, y) -> typeSubs l y >>= \v -> pure (x, v))
        newBody <-
          expSubs l body
        pure $
          e
            { AstE.inferType = newType
            , AstE.parameters = newParams
            , AstE.body = newBody
            }
    AstE.Application
      { applicationFunction
      , applicationArgument
      , inferType
      } -> do
        newType <- typeSubs l inferType
        newFunction <-
          expSubs l applicationFunction
        newArg <-
          expSubs l applicationArgument
        pure $
          e
            { AstE.inferType = newType
            , AstE.applicationFunction = newFunction
            , AstE.applicationArgument = newArg
            }
    AstE.If {inferType, condition, ifTrue, ifFalse} -> do
      newType <- typeSubs l inferType
      newCondition <-
        expSubs l condition
      newTrue <-
        expSubs l ifTrue
      newFalse <-
        expSubs l ifFalse
      pure $
        e
          { AstE.inferType = newType
          , AstE.condition = newCondition
          , AstE.ifTrue = newTrue
          , AstE.ifFalse = newFalse
          }
    AstE.Let {inferType, definitions, expression} -> do
      newType <- typeSubs l inferType
      newDef <-
        forM definitions (defSubs l)
      newExpr <-
        expSubs l expression
      pure $
        e
          { AstE.inferType = newType
          , AstE.definitions = newDef
          , AstE.expression = newExpr
          }
    AstE.Annotation {expression, _type, inferType} -> do
      newType <- typeSubs l inferType
      newType2 <- typeSubs l _type
      newExpr <-
        expSubs l expression
      pure $
        e
          { AstE.inferType = newType
          , AstE._type = newType2
          , AstE.expression = newExpr
          }


logSolver
  :: Logger :> es
  => Text
  -> Doc ann
  -> Eff es ()
logSolver header msg = debug (pretty @Text "Solver:" <> pretty header <+> Pretty.nest 2 msg)


solveExpressionType
  :: State InferenceState :> es
  => Error InferenceError :> es
  => Logger :> es
  => CstE.Expression
  -> Eff es AstE.Expression
solveExpressionType expression = do
  logSolver "Got:" $ prettyExpression pretty pretty expression
  out <- infer expression
  logSolver "constraints:" $ pretty out.constraints
  logSolver "afterInfer:" $ pretty out.expression
  subst <- findSubstitutions out.constraints
  logSolver "substitutions:" $ pretty subst
  final <- expSubs subst out.expression
  let free = freeTypeVars final
  unless
    (Set.null free)
    ( throwError $
        ExpressionContainsFreeVariablesAfterSolving
          final
          expression
          free
          subst
    )
  pure final


solveDefinitionType
  :: State InferenceState :> es
  => Error InferenceError :> es
  => Logger :> es
  => CstE.Definition
  -> Eff es AstE.Definition
solveDefinitionType cstDef = do
  (constraints, astDef) <- definitionCstToAst cstDef
  subst <- findSubstitutions constraints
  final <- defSubs subst astDef
  let free = AstE.definitionFreeTypeVars final
  unless
    (Set.null free)
    ( throwError $
        DefinitionContainsFreeVariablesAfterSolving final cstDef free subst
    )
  pure final
