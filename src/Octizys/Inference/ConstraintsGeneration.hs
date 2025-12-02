{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Octizys.Inference.ConstraintsGeneration where

import qualified Octizys.FrontEnd.Cst.Expression as CstE
import qualified Octizys.FrontEnd.Cst.Type as CstT

import Control.Applicative ((<|>))
import Control.Arrow ((<<<))
import Control.Monad (foldM)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map, lookup)
import qualified Data.Map as Map
import Data.Text (Text)
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static (Reader, asks)
import Effectful.State.Static.Local (State, gets, modify)
import Effectful.Writer.Static.Local (Writer)
import qualified Octizys.Ast.Expression as AstE
import Octizys.Ast.Type
  ( InferenceVariable (MetaVariable, UserVariable)
  , TypeVariable
  )
import qualified Octizys.Ast.Type as AstT
import Octizys.Classes.From (From (from))
import Octizys.Common.Id (ExpressionVariableId, InfoId, TypeVariableId)
import Octizys.Effects.Accumulator.Interpreter (Accumulator, accumulate)
import Octizys.Effects.IdGenerator.Effect (IdGenerator, generateId)
import Octizys.Effects.Logger.Effect (Logger, debug)
import Octizys.FrontEnd.Cst.Type
  ( Type
  , TypeVariableId
  )
import qualified Octizys.FrontEnd.Cst.Type as Cst
import Octizys.Inference.Errors
  ( Constraint (Constraint', constraintInfo, constraintType1, constraintType2)
  , ConstraintInfo (ConstraintInfo', ast, cst, reason)
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
  , InferenceError (UnboundExpressionVar)
  )
import Octizys.Pretty.FormatContext
  ( FormatContext
  , defaultFormatContext
  , formatExpressionVar
  )
import Octizys.Pretty.Formatter (Formatter (format))
import Prettyprinter (Doc, Pretty (pretty), (<+>))
import qualified Prettyprinter as Pretty
import Prelude hiding (lookup)


data KnowTypes = KnowTypes'
  { imports :: Map ExpressionVariableId (AstT.Type TypeVariable)
  , locals :: Map ExpressionVariableId (AstT.Type TypeVariable)
  }
  deriving (Show, Ord, Eq)


lookupKnowExpressionVar
  :: Reader KnowTypes :> es
  => ExpressionVariableId
  -> Eff es (Maybe (AstT.Type InferenceVariable))
lookupKnowExpressionVar var = do
  importsMap <- asks imports
  localsMap <- asks locals
  pure (from <$> (lookup var importsMap <|> lookup var localsMap))


newtype RecursiveTypes = RecursiveTypes'
  { recursiveTypes :: Map ExpressionVariableId InferenceVariable
  }
  deriving (Show, Ord, Eq)


lookupRecursiveExpressionVar
  :: Reader RecursiveTypes :> es
  => ExpressionVariableId
  -> Eff es (Maybe InferenceVariable)
lookupRecursiveExpressionVar var = do
  newsMap <- asks recursiveTypes
  pure (lookup var newsMap)


newtype InferenceErrors = InferenceErrors
  { errors :: [InferenceError]
  }
  deriving (Show, Ord, Eq)


addConstraint
  :: Accumulator Constraint :> es
  => Constraint
  -> Eff es ()
addConstraint = accumulate


freshTypeVar
  :: IdGenerator TypeVariableId :> es
  => Eff es TypeVariableId
freshTypeVar = generateId


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
      AstT.Variable (UserVariable variable)


cstParameterToAstParameter
  :: forall es
   . CstE.Parameters ExpressionVariableId TypeVariableId
  -> Eff
      es
      ( NonEmpty
          ( [Constraint]
          , ExpressionVariableId
          , AstT.Type InferenceVariable
          )
      )
cstParameterToAstParameter p = do
  pType <- lookupExpressionVar (snd p.name)
  case p of
    CstE.ParameterAlone {name = pName} ->
      pure ([], snd pName, AstT.Variable pType)
    CstE.ParameterWithType {name = pName} -> do
      let annotationType = cstToAstType p._type
          tableType = AstT.Variable pType
          constraintType =
            Constraint'
              { constraintType1 = annotationType
              , constraintType2 = tableType
              , constraintInfo =
                  ConstraintInfo'
                    { reason = ParameterTypeAnnotation
                    , cst = from p
                    , ast = from (snd pName, tableType)
                    }
              }
      pure ([constraintType], snd pName, tableType)


cstParametersToAstParameters
  :: forall es
   . CstE.Parameters ExpressionVariableId TypeVariableId
  -> Eff
      es
      ( NonEmpty
          ( [Constraint]
          , ExpressionVariableId
          , AstT.Type InferenceVariable
          )
      )
cstParametersToAstParameters params =
  mapM transParam (CstE.unParameters params)
  where
    transParam
      :: ( CstE.Parameter
            ExpressionVariableId
            TypeVariableId
         , InfoId
         )
      -> Eff es ([Constraint], ExpressionVariableId, AstT.Type InferenceVariable)
    transParam (p, _) = cstParameterToAstParameter p


definitionCstToAst
  :: ( Error InferenceError :> es
     , IdGenerator TypeVariableId :> es
     , Logger :> es
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


inferLog
  :: Logger :> es
  => Doc ann
  -> Eff es ()
inferLog d = debug (pretty @Text "Infer:" <> d)


infer
  :: Error InferenceError :> es
  => Accumulator Constraint :> es
  => IdGenerator TypeVariableId :> es
  => Logger :> es
  => CstE.Expression ExpressionVariableId TypeVariableId
  -> Eff es (AstE.Expression TypeVariable)
infer expr = do
  inferLog
    ( pretty @Text "Got:"
        <> format
          defaultFormatContext
          expr
    )
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
        eid <- lookupExpressionVar name
        maybeType <- lookupKnowType name
        inferLog
          (pretty @Text "looked " <> formatExpressionVar defaultFormatContext name)
        inferLog (pretty @Text "result " <> pretty (show maybeType))
        case maybeType of
          Just knowTy ->
            let newExp =
                  AstE.Variable
                    { name = name
                    , inferType = knowTy
                    }
                constraint =
                  Constraint'
                    { constraintType1 = AstT.Variable eid
                    , constraintType2 = knowTy
                    , constraintInfo =
                        ConstraintInfo'
                          { reason = IsKnowType
                          , cst = from expr
                          , ast = from newExp
                          }
                    }
             in do
                  addConstraint constraint
                  pure newExp
          Nothing -> do
            pure $
              AstE.Variable
                { name = name
                , inferType = AstT.Variable eid
                }
      CstE.Parens {expression} -> infer expression
      f@CstE.EFunction {} -> do
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
                    ( AstE.getType lastArgType
                        :| []
                    )
                inferredType = AstT.Arrow {start = snd value, remain = newEnd}
                outExpression =
                  from
                    AstE.Function
                      { parameters = newParams
                      , body = lastArgType
                      , inferType = inferredType
                      }
             in pure outExpression
      CstE.Application
        { applicationFunction = fun
        , applicationRemain = args
        } -> do
          initialOut <- infer fun
          foldM go initialOut args
          where
            go preOut arg = do
              domain <- AstT.Variable <$> freshTypeVar
              codomain <- AstT.Variable <$> freshTypeVar
              outArgDomain <- check arg domain ArgumentShouldBeOfDomainType
              let
                preIsArrow =
                  Constraint'
                    { constraintType1 = AstE.getType preOut
                    , constraintType2 =
                        AstT.Arrow
                          { start = domain
                          , remain = codomain :| []
                          }
                    , constraintInfo =
                        ConstraintInfo'
                          { reason = ApplicationShouldBeOnArrows
                          , cst = from expr
                          , ast = from newApp
                          }
                    }
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
              , inferType = AstE.getType thenOut
              }
          conditionIsBool =
            Constraint'
              { constraintType1 = AstE.getType condOut.expression
              , constraintType2 = from AstT.BoolType
              , constraintInfo =
                  ConstraintInfo'
                    { reason = IfCasesShouldMatch
                    , cst = from cond
                    , ast = from condOut
                    }
              }
          thenIsElse =
            Constraint'
              { constraintType1 = AstE.getType thenOut.expression
              , constraintType2 = AstE.getType elseOut.expression
              , constraintInfo =
                  ConstraintInfo'
                    { reason = IfCasesShouldMatch
                    , cst = from expr
                    , ast = from newIf
                    }
              }
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


checkLog
  :: Logger :> es
  => Doc ann
  -> Eff es ()
checkLog d = debug (pretty @Text "Check:" <> d)


check
  :: Error InferenceError :> es
  => IdGenerator TypeVariableId :> es
  => Logger :> es
  => CstE.Expression ExpressionVariableId TypeVariableId
  -> AstT.Type InferenceVariable
  -> ConstraintReason
  -> Eff es (AstE.Expression TypeVariable)
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
            Constraint'
              { constraintType1 = inferredType
              , constraintType2 = ty
              , constraintInfo =
                  ConstraintInfo'
                    { reason = reason
                    , cst = from expr
                    , ast = from inferred.expression
                    }
              }
        pure $ addConstraint newConstraint inferred
  checkLog (pretty @Text "Out:" <> format defaultFormatContext out)
  pure out
