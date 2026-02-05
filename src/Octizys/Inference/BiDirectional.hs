module Octizys.Inference.BiDirectional where

import qualified Octizys.FrontEnd.Cst.Expression as CstE
import qualified Octizys.FrontEnd.Cst.Type as CstT

import Control.Applicative ((<|>))
import Control.Arrow ((<<<))
import Control.Monad (foldM)
import qualified Data.Bifunctor as Bifunctor
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static (Reader, ask, asks, local)
import Effectful.State.Static.Local (State, gets, modify)
import Effectful.Writer.Static.Local (Writer)
import qualified Octizys.Ast.Expression as AstE
import Octizys.Ast.Type
  ( InferenceVariable
  , TypeVariable
  , instanceType
  )
import qualified Octizys.Ast.Type as AstT
import Octizys.Classes.From (From (from))
import Octizys.Common.Id (ExpressionVariableId, TypeVariableId)
import Octizys.Effects.Accumulator.Interpreter (Accumulator, accumulate)
import Octizys.Effects.IdGenerator.Effect (IdGenerator, generateId)
import Octizys.FrontEnd.Cst.Type
  ( Type
  , TypeVariableId
  )
import qualified Octizys.FrontEnd.Cst.Type as Cst
import Octizys.Inference.Constraint
  ( Constraint
  , ConstraintInfo
  , ConstraintReason
    ( ApplicationShouldBeOnArrows
    , ArgumentShouldBeOfDomainType
    , DefinitionTypeAnnotation
    , DefinitionTypeAnnotationWithArgs
    , DefinitionVariableAndBodyShouldBeEqual
    , IfCasesShouldMatch
    , IsKnowType
    , ParameterTypeAnnotation
    , TypeAnnotation
    )
  , makeConstraint
  , makeConstraintInfo
  )
import Octizys.Inference.Context (Context)
import qualified Octizys.Inference.Context as Context
import Octizys.Logging.Effect (Log)
import qualified Octizys.Logging.Loggers as Log
import Prettyprinter (Doc, Pretty (pretty), (<+>))
import qualified Prettyprinter as Pretty
import Prelude hiding (lookup)


addConstraint
  :: Accumulator Constraint :> es
  => Constraint
  -> Eff es ()
addConstraint = accumulate


lookup
  :: Error Text :> es
  => Reader Context :> es
  => ExpressionVariableId
  -> Eff es (AstT.Type InferenceVariable)
lookup ev = do
  ctx <- ask
  Context.lookup ctx ev


freshTypeVar
  :: IdGenerator TypeVariableId :> es
  => Eff es TypeVariableId
freshTypeVar = generateId Nothing


freshMonoVar
  :: IdGenerator TypeVariableId :> es
  => Eff es (AstT.MonoType AstT.InferenceVariable)
freshMonoVar =
  (AstT.Variable <<< AstT.RealTypeVariable) <$> freshTypeVar


freshInferenceVar
  :: IdGenerator TypeVariableId :> es
  => Eff es (AstT.Type AstT.InferenceVariable)
freshInferenceVar =
  AstT.TMono <$> freshMonoVar


cstToAstMonoType
  :: Type TypeVariableId
  -> AstT.MonoType InferenceVariable
cstToAstMonoType t =
  case t of
    Cst.BoolType {} -> from AstT.BoolType
    Cst.IntType {} -> from AstT.IntType
    Cst.Arrow {start, remain} ->
      let newStart = cstToAstMonoType start
          newRemain = cstToAstMonoType <$> (snd <$> remain)
       in AstT.Arrow
            { start = newStart
            , remain = newRemain
            }
    Cst.Parens {_type} -> cstToAstMonoType _type
    Cst.TVariable {variable} ->
      AstT.Variable (AstT.RealTypeVariable variable)


cstParametersToAstParameters
  :: forall es
   . CstE.Parameters ExpressionVariableId TypeVariableId
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


definitionCstToAst
  :: ( Error Text :> es
     , IdGenerator TypeVariableId :> es
     , Log :> es
     )
  => CstE.Definition ExpressionVariableId TypeVariableId
  -> Eff es ([Constraint], AstE.Definition InferenceVariable)
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
             in [ Constraint'
                    { constraintType1 = newBodyType
                    , constraintType2 = newExpectedType
                    , constraintInfo =
                        ConstraintInfo'
                          { reason = DefinitionTypeAnnotation
                          , cst = from d
                          , ast =
                              from
                                ( AstE.Variable
                                    (snd d.name)
                                    newExpectedType
                                )
                          }
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
            Constraint'
              { constraintType1 = AstT.Variable nameTypeVar
              , constraintType2 = newBodyType
              , constraintInfo =
                  ConstraintInfo'
                    { reason = DefinitionVariableAndBodyShouldBeEqual
                    , cst = from d
                    , ast = from newExp
                    }
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
                from
                  AstE.Function
                    { parameters = (\(_, x, y) -> (x, y)) <$> newParams
                    , body = newBody
                    , inferType = newType
                    }
            }
        -- `f ~ a -> b -> c`
        fullDefinitionConstraint =
          Constraint'
            { constraintType1 = AstT.Variable nameTypeVar
            , constraintType2 = newType
            , -- TODO: adjust this, in case we have outputType we
              -- should choose it.
              constraintInfo =
                ConstraintInfo'
                  { reason = DefinitionTypeAnnotationWithArgs
                  , cst = from d
                  , ast = from newExp
                  }
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


infer
  :: Error Text :> es
  => Reader Context :> es
  => Accumulator Constraint :> es
  => IdGenerator TypeVariableId :> es
  => Log :> es
  => CstE.Expression ExpressionVariableId TypeVariableId
  -> Eff es (AstE.Expression InferenceVariable)
infer expr = do
  out <-
    case expr of
      CstE.EInt {intValue} -> do
        pure $
          from
            AstE.VInt
              { intValue = intValue
              , inferType = from AstT.IntType
              }
      CstE.EBool {boolValue} -> do
        pure $
          from
            AstE.VBool
              { boolValue = boolValue
              , inferType = from AstT.BoolType
              }
      CstE.Variable {name} -> do
        ty <- lookup name
        tyClosed <- instanceType ty
        pure $
          AstE.Variable
            { name = name
            , inferType = tyClosed
            }
      CstE.Parens {expression} -> infer expression
      f@CstE.EFunction {} -> do
        -- Generate type vars for every parameter
        newParams <- cstParametersToAstParameters f.params

        -- Extend the context with the new type vars
        context <- ask
        extendedContext <-
          Context.addExpressionVars
            (NonEmpty.toList (Bifunctor.second from <$> newParams))
            context

        -- Infer the body with the new type vars
        newBody <- local (const extendedContext) (infer f.body)
        let
          newBodyType = newBody.inferType
        case snd <$> newParams of
          value :| remain ->
            let
              -- Construct the arrow
              newEnd =
                case NonEmpty.nonEmpty remain of
                  Just nonEmptyRemain ->
                    nonEmptyRemain <> NonEmpty.singleton newBodyType
                  Nothing -> NonEmpty.singleton newBodyType
              inferredType = AstT.Arrow {start = value, remain = newEnd}
              outExpression =
                from
                  AstE.Function
                    { parameters = newParams
                    , body = newBody
                    , inferType = inferredType
                    }
             in
              pure outExpression
      CstE.Application
        { applicationFunction = fun
        , applicationRemain = args
        } -> do
          initialOut <- infer fun
          foldM go initialOut args
          where
            go preOut arg = do
              domain <- freshMonoVar
              codomain <- freshMonoVar
              outArgDomain <- check arg (from domain) ArgumentShouldBeOfDomainType
              let
                preIsArrow =
                  makeConstraint
                    (AstE.getMonoType preOut)
                    ( AstT.Arrow
                        { start = domain
                        , remain = codomain :| []
                        }
                    )
                    ( makeConstraintInfo
                        ApplicationShouldBeOnArrows
                        (from expr)
                        (from newApp)
                    )
                newApp =
                  AstE.Application
                    { applicationFunction = preOut
                    , applicationArgument = outArgDomain
                    , inferType = codomain
                    }
              addConstraint preIsArrow
              pure newApp
      CstE.If {condition = cond, ifTrue, ifFalse, _then, _else} -> do
        condOut <- infer cond
        thenOut <- infer ifTrue
        elseOut <- infer ifFalse
        let
          newIf =
            AstE.If
              { condition = condOut
              , ifTrue = thenOut
              , ifFalse = elseOut
              , inferType = AstE.getMonoType thenOut
              }
          conditionIsBool =
            makeConstraint
              (AstE.getMonoType condOut.expression)
              (from AstT.BoolType)
              ( makeConstraintInfo
                  IfCasesShouldMatch
                  (from cond)
                  (from condOut)
              )
          thenIsElse =
            makeConstraint
              (AstE.getMonoType thenOut.expression)
              (AstE.getMonoType elseOut.expression)
              ( makeConstraintInfo
                  IfCasesShouldMatch
                  (from expr)
                  (from newIf)
              )
        addConstraint thenIsElse
        addConstraint conditionIsBool
        pure newIf
      CstE.Let {definitions, expression = _in} -> do
        defs <- mapM definitionCstToAst (fst <$> definitions)
        inOut <- infer _in
        let
          newExp =
            AstE.Let
              { definitions = snd <$> defs
              , expression = inOut
              , inferType = AstE.getType inOut
              }
         in
          pure newExp
      CstE.Annotation {expression = exprr, _type = ann} -> do
        let newTypeAnn = cstToAstType ann
        check exprr newTypeAnn TypeAnnotation
  inferLog (pretty @Text "Out:" <> format defaultFormatContext out)
  pure out


check
  :: Error String :> es
  => IdGenerator TypeVariableId :> es
  => Log :> es
  => CstE.Expression ExpressionVariableId TypeVariableId
  -> AstT.MonoType InferenceVariable
  -> ConstraintReason
  -> Eff es (AstE.Expression InferenceVariable)
check expr ty reason = do
  checkLog
    ( pretty @Text "Got exp:"
        <> format defaultFormatContext expr
    )
  checkLog (pretty @Text "Got type:" <> format defaultFormatContext ty)
  out <-
    case (expr, ty) of
      (CstE.EInt {intValue}, AstT.VType AstT.IntType) ->
        pure $
          from
            AstE.VInt
              { intValue =
                  intValue
              , inferType = from AstT.IntType
              }
      (CstE.EBool {boolValue}, AstT.VType AstT.BoolType) ->
        pure $
          from
            AstE.VBool
              { boolValue =
                  boolValue
              , inferType = from AstT.BoolType
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
            makeConstraint
              inferredType
              ty
              ( makeConstraintInfo
                  reason
                  (from expr)
                  (from inferred.expression)
              )
        pure $ addConstraint newConstraint inferred
  checkLog (pretty @Text "Out:" <> format defaultFormatContext out)
  pure out
