module Octizys.Inference.BiDirectional where

import qualified Octizys.FrontEnd.Cst.Expression as CstE
import qualified Octizys.FrontEnd.Cst.Type as CstT

import Control.Arrow ((<<<))
import Control.Monad (foldM, when)
import qualified Data.Bifunctor as Bifunctor
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import Data.Text (Text)

import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, HasCallStack, tryError)
import Effectful.Reader.Static (Reader, ask, local)
import Effectful.State.Static.Local (State, get, put, runState)

import Octizys.Ast.Expression (getMonoType)
import qualified Octizys.Ast.Expression as AstE
import Octizys.Ast.Type (instanceType)
import qualified Octizys.Ast.Type as AstT
import Octizys.Ast.Type.Basics (InferenceVariable)
import qualified Octizys.Ast.Type.Basics as AstT
import Octizys.Ast.Type.MonoType (hasTypeVar)
import qualified Octizys.Ast.Type.MonoType as AstT
import qualified Octizys.Ast.Type.Scheme as AstT
import Octizys.Classes.From (From (from))
import Octizys.Common.Id (ExpressionVariableId, TypeVariableId)
import Octizys.Effects.Accumulator.Interpreter (Accumulator, accumulate)
import Octizys.Effects.IdGenerator.Effect (IdGenerator, generateId)
import Octizys.Format.Class (Formattable (format))
import Octizys.Format.Config (defaultConfiguration)
import qualified Octizys.Format.Utils as Format
import Octizys.FrontEnd.Cst.Expression
  ( BoolExpression (BoolExpression')
  , IntExpression (IntExpression')
  )
import Octizys.FrontEnd.Cst.SourceInfo (SourceInfo)
import Octizys.FrontEnd.Cst.Type
  ( Type
  )
import qualified Octizys.FrontEnd.Cst.Type as Cst
import Octizys.Inference.Constraint
  ( Constraint
  , ConstraintId
  , ConstraintReason
    ( ApplicationShouldBeOnArrows
    , ArgumentShouldBeOfDomainType
    , IfCasesShouldMatch
    , IfConditionShouldBeBool
    , TypeAnnotation
    )
  , makeConstraint
  , makeConstraintFromParent
  , makeConstraintInfo
  )
import Octizys.Inference.Context (Context)
import qualified Octizys.Inference.Context as Context
import Octizys.Inference.Substitution (Substitution)
import qualified Octizys.Inference.Substitution as Substitution
import Octizys.Logging.Effect (Log)
import Prettyprinter (Pretty (pretty), (<+>))
import qualified Prettyprinter as Pretty
import Prelude hiding (lookup)


addConstraint
  :: Accumulator Constraint :> es
  => HasCallStack
  => Constraint
  -> Eff es ()
addConstraint = accumulate


lookup
  :: Error Text :> es
  => HasCallStack
  => Reader Context :> es
  => ExpressionVariableId
  -> Eff es (AstT.Type InferenceVariable)
lookup ev = do
  ctx <- ask
  Context.lookup ctx ev


freshTypeVar
  :: IdGenerator TypeVariableId :> es
  => HasCallStack
  => Eff es TypeVariableId
freshTypeVar = generateId Nothing


freshMonoVar
  :: IdGenerator TypeVariableId :> es
  => HasCallStack
  => Eff es (AstT.MonoType AstT.InferenceVariable)
freshMonoVar =
  (AstT.MonoVariable <<< AstT.RealTypeVariable) <$> freshTypeVar


freshInferenceVar
  :: IdGenerator TypeVariableId :> es
  => HasCallStack
  => Eff es (AstT.Type AstT.InferenceVariable)
freshInferenceVar =
  AstT.TMono <$> freshMonoVar


cstToAstMonoType
  :: Type TypeVariableId
  -> AstT.MonoType InferenceVariable
cstToAstMonoType t =
  case t of
    Cst.TBool {} -> from AstT.BoolType
    Cst.TInt {} -> from AstT.IntType
    Cst.TArrow (Cst.Arrow' {start, remain}) ->
      let newStart = cstToAstMonoType start
          newRemain = cstToAstMonoType <$> (snd <$> remain)
       in AstT.MonoArrow
            AstT.Arrow'
              { start = newStart
              , remain = newRemain
              }
    Cst.TParens (Cst.Parens' {_type}) -> cstToAstMonoType _type
    Cst.TVariable (Cst.Variable' {variable}) ->
      AstT.MonoVariable (AstT.RealTypeVariable variable)


cstParametersToAstParameters
  :: forall es
   . IdGenerator TypeVariableId :> es
  => HasCallStack
  => CstE.Parameters ExpressionVariableId TypeVariableId
  -> Eff
      es
      ( NonEmpty
          ( ExpressionVariableId
          , AstT.MonoType InferenceVariable
          )
      )
cstParametersToAstParameters params =
  let
    items :: NonEmpty (ExpressionVariableId, Maybe (CstT.Type TypeVariableId))
    items = CstE.getAnnotatedParameters params
    inferParam
      :: (ExpressionVariableId, Maybe (CstT.Type TypeVariableId))
      -> Eff es (ExpressionVariableId, AstT.MonoType InferenceVariable)
    inferParam (n, mty) = do
      ty <- maybe freshMonoVar (pure <<< cstToAstMonoType) mty
      pure (n, ty)
   in
    mapM inferParam items


definitionAnnotationToAst
  :: forall es
   . IdGenerator TypeVariableId :> es
  => HasCallStack
  => CstE.DefinitionTypeAnnotation ExpressionVariableId TypeVariableId
  -> Eff es (AstT.Type InferenceVariable)
definitionAnnotationToAst ann =
  let
    newOutput = cstToAstMonoType ann.outputType
   in
    case ann.schemeStart of
      Just (CstE.SchemeStart' _ varsWithSource _) ->
        let vars = snd <$> varsWithSource
         in case ann.parameters of
              Just params -> do
                paramTypesWithNames <- cstParametersToAstParameters params
                let paramTypes = snd <$> paramTypesWithNames
                    newSchemeBody = AstT.MonoArrow $ AstT.arrowFromArgsAndOutput paramTypes newOutput
                pure $ from $ AstT.Scheme' vars newSchemeBody
              Nothing ->
                pure $
                  from $
                    AstT.Scheme' vars newOutput
      Nothing ->
        case ann.parameters of
          Just params -> do
            paramTypesWithNames <- cstParametersToAstParameters params
            let paramTypes = snd <$> paramTypesWithNames
            pure $
              from $
                AstT.MonoArrow (AstT.arrowFromArgsAndOutput paramTypes newOutput)
          Nothing ->
            pure $ from newOutput


{- | The first item in the tuple are types with annotations
In the second item are the types that where instanced with
mono variables.
-}
genTypesForDefinitions
  :: forall es
   . IdGenerator TypeVariableId :> es
  => HasCallStack
  => NonEmpty (CstE.Definition ExpressionVariableId TypeVariableId)
  -> Eff
      es
      ( [(ExpressionVariableId, AstT.Type InferenceVariable)]
      , [(ExpressionVariableId, AstT.Type InferenceVariable)]
      )
genTypesForDefinitions ds = go (NonEmpty.toList ds) [] []
  where
    go
      :: [CstE.Definition ExpressionVariableId TypeVariableId]
      -> [(ExpressionVariableId, AstT.Type InferenceVariable)]
      -> [(ExpressionVariableId, AstT.Type InferenceVariable)]
      -> Eff
          es
          ( [(ExpressionVariableId, AstT.Type InferenceVariable)]
          , [(ExpressionVariableId, AstT.Type InferenceVariable)]
          )
    go [] acc1 acc2 = pure (acc1, acc2)
    go (x : xs) acc1 acc2 =
      case x._type of
        Just ann -> do
          astAnnotation <- definitionAnnotationToAst ann
          go xs ((snd x.name, astAnnotation) : acc1) acc2
        Nothing -> do
          astAnnotation <- freshInferenceVar
          go xs acc1 ((snd x.name, astAnnotation) : acc2)


generalize
  :: Reader Context :> es
  => HasCallStack
  => AstT.Type InferenceVariable
  -> Eff es (AstT.Type InferenceVariable)
generalize = undefined


inferDefinitions
  :: Error Text :> es
  => HasCallStack
  => Reader Context :> es
  => Accumulator Constraint :> es
  => IdGenerator TypeVariableId :> es
  => State ConstraintId :> es
  => State Substitution :> es
  => Log :> es
  => NonEmpty (CstE.Definition ExpressionVariableId TypeVariableId, SourceInfo)
  -> Eff
      es
      ( NonEmpty (ExpressionVariableId, AstE.Expression InferenceVariable)
      , Context
      )
inferDefinitions definitions = do
  (annotatedBinds, monoBinds) <- genTypesForDefinitions (fst <$> definitions)
  originalContext <- ask
  extendedWithPoly <-
    Context.addExpressionVars
      annotatedBinds
      originalContext
  extendedWithBoth <-
    Context.addExpressionVars
      monoBinds
      extendedWithPoly
  inferredBodies <-
    -- TODO:FIXME, to infer the bodies of annotated expressions, one needs to add the binded variables to the context of binded variables first. Additionally, use check for them instead of infer.
    local
      (const extendedWithBoth)
      ( mapM
          ( \(d, _) -> do
              bodyType <- infer d.definition
              pure (snd d.name, bodyType)
          )
          definitions
      )
  currentSubstitution <- get @Substitution
  newGeneralizedBinds <-
    mapM
      ( \(n, t) -> do
          newT <- generalize (Substitution.applyToType currentSubstitution t)
          pure (n, newT)
      )
      monoBinds
  finalContext <-
    Context.addExpressionVars
      newGeneralizedBinds
      extendedWithPoly
  pure (inferredBodies, finalContext)


inferVariable
  :: forall es
   . Error Text :> es
  => HasCallStack
  => Reader Context :> es
  => Accumulator Constraint :> es
  => IdGenerator TypeVariableId :> es
  => State ConstraintId :> es
  => State Substitution :> es
  => Log :> es
  => CstE.Variable ExpressionVariableId
  -> Eff es (AstE.Expression InferenceVariable)
inferVariable (CstE.Variable' {name}) = do
  ty <- lookup name
  tyClosed <- instanceType ty
  pure $
    AstE.EVariable $
      AstE.Variable'
        { name = name
        , inferType = tyClosed
        }


inferFunction
  :: forall es
   . Error Text :> es
  => HasCallStack
  => Reader Context :> es
  => Accumulator Constraint :> es
  => IdGenerator TypeVariableId :> es
  => State ConstraintId :> es
  => State Substitution :> es
  => Log :> es
  => CstE.Function ExpressionVariableId TypeVariableId
  -> Eff es (AstE.Expression InferenceVariable)
inferFunction f@(CstE.Function' {}) = do
  -- Generate type vars for every parameter
  newParams <- cstParametersToAstParameters f.parameters

  -- Extend the context with the new type vars
  context <- ask
  extendedContext <-
    Context.addExpressionVars
      (NonEmpty.toList (Bifunctor.second from <$> newParams))
      context

  -- Infer the body with the new type vars
  newBody <- local (const extendedContext) (infer f.body)
  let
    newBodyType = getMonoType newBody
  case snd <$> newParams of
    value :| remain ->
      let
        -- Construct the arrow
        newEnd =
          case NonEmpty.nonEmpty remain of
            Just nonEmptyRemain ->
              nonEmptyRemain <> NonEmpty.singleton newBodyType
            Nothing -> NonEmpty.singleton newBodyType
        inferredType = AstT.Arrow' {start = value, remain = newEnd}
        outExpression =
          from $
            from @(AstE.Value InferenceVariable) $
              AstE.Function'
                { parameters = newParams
                , body = newBody
                , inferType = inferredType
                }
       in
        pure outExpression


inferApplication
  :: forall es
   . Error Text :> es
  => HasCallStack
  => Reader Context :> es
  => Accumulator Constraint :> es
  => IdGenerator TypeVariableId :> es
  => State ConstraintId :> es
  => State Substitution :> es
  => Log :> es
  => CstE.Application ExpressionVariableId TypeVariableId
  -> Eff es (AstE.Expression InferenceVariable)
inferApplication
  expr@( CstE.Application'
          { function = fun
          , remain = args
          }
        ) = do
    initialOut <- infer fun
    (_, out) <- foldM go (Nothing, initialOut) args
    pure out
    where
      go
        :: ( Maybe Constraint
           , AstE.Expression InferenceVariable
           )
        -> CstE.Expression ExpressionVariableId TypeVariableId
        -> Eff es (Maybe Constraint, AstE.Expression InferenceVariable)
      go (maybeConstraint, preOut) arg = do
        domain <- freshMonoVar
        codomain <- freshMonoVar
        outArgDomain <- check arg domain ArgumentShouldBeOfDomainType
        let
          newApp =
            from $
              AstE.Application'
                { function = preOut
                , argument = outArgDomain
                , inferType = codomain
                }
        constraintInfo <-
          makeConstraintInfo
            ApplicationShouldBeOnArrows
            ( from $
                from @(CstE.Expression ExpressionVariableId TypeVariableId)
                  expr
            )
            (from newApp)
            maybeConstraint
            []
        let
          preIsArrow =
            makeConstraint
              (AstE.getMonoType preOut)
              ( AstT.MonoArrow
                  AstT.Arrow'
                    { start = domain
                    , remain = codomain :| []
                    }
              )
              constraintInfo
        addConstraint preIsArrow
        unify preIsArrow
        pure (Just preIsArrow, newApp)


inferIf
  :: forall es
   . Error Text :> es
  => HasCallStack
  => Reader Context :> es
  => Accumulator Constraint :> es
  => IdGenerator TypeVariableId :> es
  => State ConstraintId :> es
  => State Substitution :> es
  => Log :> es
  => CstE.If ExpressionVariableId TypeVariableId
  -> Eff es (AstE.Expression InferenceVariable)
inferIf expr@CstE.If' {condition = cond, ifTrue, ifFalse, _then, _else} = do
  condOut <- infer cond
  thenOut <- infer ifTrue
  elseOut <- infer ifFalse
  let
    newIf =
      from $
        AstE.If'
          { condition = condOut
          , ifTrue = thenOut
          , ifFalse = elseOut
          , inferType = AstE.getMonoType thenOut
          }
  conditionIsBoolInfo <-
    makeConstraintInfo
      IfConditionShouldBeBool
      (from cond)
      (from condOut)
      Nothing
      []
  thenIsElseInfo <-
    makeConstraintInfo
      IfCasesShouldMatch
      (from $ from @(CstE.Expression ExpressionVariableId TypeVariableId) expr)
      (from newIf)
      Nothing
      []
  let
    conditionIsBool =
      makeConstraint
        (AstE.getMonoType condOut)
        (from AstT.BoolType)
        conditionIsBoolInfo
    thenIsElse =
      makeConstraint
        (AstE.getMonoType thenOut)
        (AstE.getMonoType elseOut)
        thenIsElseInfo
  addConstraint conditionIsBool
  addConstraint thenIsElse
  unify conditionIsBool
  unify thenIsElse
  pure newIf


inferLet
  :: forall es
   . Error Text :> es
  => HasCallStack
  => Reader Context :> es
  => Accumulator Constraint :> es
  => IdGenerator TypeVariableId :> es
  => State ConstraintId :> es
  => State Substitution :> es
  => Log :> es
  => CstE.Let ExpressionVariableId TypeVariableId
  -> Eff es (AstE.Expression InferenceVariable)
inferLet CstE.Let' {definitions, expression = _in} = do
  (inferredBodies, finalContext) <-
    inferDefinitions definitions
  newInBody <- local (const finalContext) (infer _in)
  pure
    ( from $
        AstE.Let'
          { definitions =
              (\(n, d) -> AstE.Definition' n d (from $ getMonoType d)) <$> inferredBodies
          , expression = newInBody
          , inferType = getMonoType newInBody
          }
    )


inferAnnotation
  :: forall es
   . Error Text :> es
  => HasCallStack
  => Reader Context :> es
  => Accumulator Constraint :> es
  => IdGenerator TypeVariableId :> es
  => State ConstraintId :> es
  => State Substitution :> es
  => Log :> es
  => CstE.Annotation ExpressionVariableId TypeVariableId
  -> Eff es (AstE.Expression InferenceVariable)
inferAnnotation CstE.Annotation' {expression = exprr, _type = ann} = do
  let newTypeAnn = cstToAstMonoType ann
  check exprr newTypeAnn TypeAnnotation


infer
  :: forall es
   . Error Text :> es
  => HasCallStack
  => Reader Context :> es
  => Accumulator Constraint :> es
  => IdGenerator TypeVariableId :> es
  => State ConstraintId :> es
  => State Substitution :> es
  => Log :> es
  => CstE.Expression ExpressionVariableId TypeVariableId
  -> Eff es (AstE.Expression InferenceVariable)
infer expr =
  case expr of
    CstE.EInt (IntExpression' {value}) -> do
      pure $
        AstE.EValue $
          AstE.VInt $
            AstE.ValueInt' {value}
    CstE.EBool (BoolExpression' {value}) -> do
      pure $
        AstE.EValue $
          AstE.VBool $
            AstE.ValueBool' {value}
    CstE.EVariable v -> inferVariable v
    CstE.EParens (CstE.Parens' {expression}) -> infer expression
    CstE.EFunction v -> inferFunction v
    CstE.EApplication v -> inferApplication v
    CstE.EIf v -> inferIf v
    CstE.ELet v -> inferLet v
    CstE.EAnnotation v -> inferAnnotation v


check
  :: Error Text :> es
  => HasCallStack
  => Reader Context :> es
  => Accumulator Constraint :> es
  => IdGenerator TypeVariableId :> es
  => State ConstraintId :> es
  => State Substitution :> es
  => Log :> es
  => CstE.Expression ExpressionVariableId TypeVariableId
  -> AstT.MonoType InferenceVariable
  -> ConstraintReason
  -> Eff es (AstE.Expression InferenceVariable)
check expr ty cr =
  case (expr, ty) of
    (CstE.EInt (CstE.IntExpression' {value}), AstT.MonoValue AstT.IntType) ->
      pure $
        from $
          from @(AstE.Value InferenceVariable) $
            AstE.ValueInt'
              { value =
                  value
              }
    (CstE.EBool (CstE.BoolExpression' {value}), AstT.MonoValue AstT.BoolType) ->
      pure $
        from $
          from @(AstE.Value InferenceVariable) $
            AstE.ValueBool'
              { value =
                  value
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
          AstE.getMonoType inferred
      constraintInfo <-
        makeConstraintInfo
          cr
          (from expr)
          (from inferred)
          Nothing
          []
      let
        newConstraint =
          makeConstraint
            inferredType
            ty
            constraintInfo
      addConstraint newConstraint
      unify newConstraint
      pure inferred


occursCheckAndAddtoSubs
  :: State Substitution :> es
  => HasCallStack
  => Error Text :> es
  => InferenceVariable
  -> AstT.MonoType InferenceVariable
  -> Eff es ()
occursCheckAndAddtoSubs v t =
  if hasTypeVar v t
    then
      Format.throwDocError $
        Format.text "Error, the variable "
          <+> pretty v
          <+> Format.text "occurs on the type"
          <+> format defaultConfiguration t
    else do
      currentSubstitution <- get
      case v of
        AstT.RealTypeVariable tv ->
          let
            localSub = Substitution.singleton tv t
            newSub = Substitution.compose localSub currentSubstitution
           in
            put newSub
        _ -> pure ()


makeRemainArrowConstraints
  :: State ConstraintId :> es
  => HasCallStack
  => NonEmpty (AstT.MonoType InferenceVariable)
  -> NonEmpty (AstT.MonoType InferenceVariable)
  -> Constraint
  -> Eff es (NonEmpty Constraint)
makeRemainArrowConstraints (x0 :| xs0) (y0 :| ys0) c =
  go x0 xs0 y0 ys0
  where
    go last1 xs last2 ys =
      case (xs, ys) of
        (x : xs', y : ys') -> do
          c1 <- makeConstraintFromParent last1 last2 c
          rest <- go x xs' y ys'
          pure (c1 :| NonEmpty.toList rest)
        ([], y : ys') -> do
          let rhs = AstT.MonoArrow $ AstT.Arrow' last2 (y :| ys')
          c1 <- makeConstraintFromParent last1 rhs c
          pure (c1 :| [])
        (x : xs', []) -> do
          let lhs = AstT.MonoArrow $ AstT.Arrow' last1 (x :| xs')
          c1 <- makeConstraintFromParent lhs last2 c
          pure (c1 :| [])
        ([], []) -> do
          c1 <- makeConstraintFromParent last1 last2 c
          pure (c1 :| [])


unify
  :: Reader Context :> es
  => HasCallStack
  => Accumulator Constraint :> es
  => IdGenerator TypeVariableId :> es
  => State Substitution :> es
  => State ConstraintId :> es
  => Error Text :> es
  => Log :> es
  => Constraint
  -> Eff es ()
unify c = do
  currentSubstitution <- get @Substitution
  let
    (tlStart, trStart)
      :: (AstT.MonoType InferenceVariable, AstT.MonoType InferenceVariable) = from c
    tl = Substitution.applyToMonoType currentSubstitution tlStart
    tr = Substitution.applyToMonoType currentSubstitution trStart
  case (tl, tr) of
    (AstT.MonoVariable v1, AstT.MonoVariable v2) ->
      when
        (v1 /= v2)
        ( occursCheckAndAddtoSubs v1 tr
        )
    (AstT.MonoVariable v1, _) ->
      occursCheckAndAddtoSubs v1 tr
    (_, AstT.MonoVariable v1) ->
      occursCheckAndAddtoSubs v1 tl
    (AstT.MonoValue AstT.BoolType, AstT.MonoValue AstT.BoolType) -> pure ()
    (AstT.MonoValue AstT.IntType, AstT.MonoValue AstT.IntType) -> pure ()
    ( AstT.MonoArrow (AstT.Arrow' in1 out1)
      , AstT.MonoArrow (AstT.Arrow' in2 out2)
      ) -> do
        inConstraint <- makeConstraintFromParent in1 in2 c
        unify inConstraint
        accumulate inConstraint

        newSubs <- get @Substitution

        let
          newOut1 = Substitution.applyToMonoType newSubs <$> out1
          newOut2 = Substitution.applyToMonoType newSubs <$> out2

        outConstraints <- makeRemainArrowConstraints newOut1 newOut2 c
        mapM_ accumulate outConstraints
        mapM_ unify outConstraints
    (_, _) ->
      Format.throwDocError $
        Format.text "Error, can't unify in constraint"
          <> Pretty.line
          <> Format.indentFormat defaultConfiguration c


solveExpressionTypeAddInfo
  :: forall es
   . Error Text :> es
  => HasCallStack
  => Reader Context :> es
  => Accumulator Constraint :> es
  => IdGenerator TypeVariableId :> es
  => State ConstraintId :> es
  => Log :> es
  => CstE.Expression ExpressionVariableId TypeVariableId
  -> Eff
      es
      ( AstE.Expression AstT.TypeVariable
      , Map TypeVariableId (AstT.MonoType AstT.TypeVariable)
      )
solveExpressionTypeAddInfo expr = do
  (inferredExpression, initialSubs) <-
    runState Substitution.empty $ infer expr
  maybeSubs <- tryError $ Substitution.finalizeSubstitution initialSubs
  case maybeSubs of
    Left (_, originalMsg) ->
      Format.throwDocError
        ( Format.text "Error while trying to finalize the substitution!"
            <> Format.nest
              defaultConfiguration
              ( Pretty.hardline
                  <> Format.text originalMsg
                  <> Format.formatWithHeader
                    defaultConfiguration
                    "Annotated AST:"
                    inferredExpression
                  <> Format.formatWithHeader
                    defaultConfiguration
                    "Substitution not finalized:"
                    initialSubs
              )
        )
    Right subs -> do
      maybeFinalExpression <-
        tryError
          ( Substitution.apply
              subs
              inferredExpression
          )
      case maybeFinalExpression of
        Right out -> pure (out, subs)
        Left (_, originalMsg) ->
          Format.throwDocError
            ( Format.text
                "Error while trying apply the final substitution to the annotated expression!"
                <> Format.nest
                  defaultConfiguration
                  ( Pretty.hardline
                      <> Format.text originalMsg
                      <> Pretty.hardline
                      <> Format.formatWithHeader
                        defaultConfiguration
                        "Annotated AST:"
                        inferredExpression
                      <> Pretty.hardline
                      <> Format.text "Finalized substitution:"
                      <> Pretty.hardline
                      <> Format.formatMap defaultConfiguration subs
                  )
            )


solveExpressionType
  :: forall es
   . Error Text :> es
  => HasCallStack
  => Reader Context :> es
  => Accumulator Constraint :> es
  => IdGenerator TypeVariableId :> es
  => State ConstraintId :> es
  => Log :> es
  => CstE.Expression ExpressionVariableId TypeVariableId
  -> Eff es (AstE.Expression AstT.TypeVariable)
solveExpressionType expr = fst <$> solveExpressionTypeAddInfo expr
