{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Octizys.Inference.ConstraintsGeneration where

import qualified Octizys.Cst.Expression as CstE
import qualified Octizys.Cst.Type as CstT

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
import qualified Octizys.Ast.Expression as AstE
import Octizys.Ast.Type (InferenceVariable (MetaVariable), TypeVariable)
import qualified Octizys.Ast.Type as AstT
import Octizys.Classes.From (From (from))
import Octizys.Common.Id (ExpressionVariableId, InfoId, TypeVariableId)
import Octizys.Cst.Type
  ( Type
  , TypeVariableId
  )
import qualified Octizys.Cst.Type as Cst
import Octizys.Effects.IdGenerator.Effect (IdGenerator, generateId)
import Octizys.Effects.Logger.Effect (Logger, debug)
import Octizys.Effects.SymbolResolution.Effect
  ( SymbolResolutionState (expVarTable)
  )
import Octizys.Effects.SymbolResolution.Interpreter
  ( SourceExpressionVariableInfo (typeId)
  )
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


data Output = Output'
  { constraints :: [Constraint]
  , expression :: AstE.Expression InferenceVariable
  }
  deriving (Show, Ord, Eq)


instance Formatter ann (FormatContext ann) Output where
  format ctx Output' {constraints, expression} =
    Pretty.parens
      ( Pretty.list (format ctx <$> constraints)
          <+> ","
          <+> format ctx expression
      )


addConstraint :: Constraint -> Output -> Output
addConstraint c o =
  o {constraints = c : o.constraints}


freshTypeVar
  :: IdGenerator TypeVariableId :> es
  => Eff es TypeVariableId
freshTypeVar = generateId


data InferenceState = InferenceState'
  { knowTypes :: Map ExpressionVariableId (AstT.Type TypeVariable)
  -- ^ Symbols that were defined on imports or on a previous pass of
  -- the interpreter.
  , localDefinedSymbols
      :: Map ExpressionVariableId (AstT.Type InferenceVariable)
  }
  deriving (Show, Ord, Eq)


lookupExpressionVar
  :: ( Error InferenceError :> es
     , State InferenceState :> es
     )
  => ExpressionVariableId
  -> Eff es (AstT.Type InferenceVariable)
lookupExpressionVar var = do
  knowMap <- gets knowTypes
  newsMap <- gets localDefinedSymbols
  case (snd <$> lookup var knowMap)
    <|> ((from <<< snd) <$> lookup var newsMap) of
    Just value -> pure value
    Nothing -> do
      newTypeVar <- freshTypeVar
      let newType :: AstT.Type InferenceVariable = from $ MetaVariable newTypeVar
      modify (\s -> s {localDefinedSymbols = (var, newType)})
      pure newType


initialInferenceState :: InferenceState
initialInferenceState =
  InferenceState'
    { knowTypes = mempty
    }


insertKnowType
  :: State InferenceState :> es
  => ExpressionVariableId
  -> AstT.Type InferenceVariable
  -> Eff es ()
insertKnowType vid t = do
  modify (\s -> s {knowTypes = Map.insert vid (vid, t) s.knowTypes})


{- | For use in this case:
let x : forall a b c . ... = ...
To register the variables a b c
-}
insertNewDefinitionType
  :: State InferenceState :> es
  => ExpressionVariableId
  -> AstT.Type InferenceVariable
  -> Eff es TypeVariableId
insertNewDefinitionType =
  modify (\s -> s {knowTypes = Map.insert vid (vid, t) s.knowTypes})


cstToAstType
  :: Type
  -> AstT.Type InferenceVariable
cstToAstType t =
  case t of
    Cst.BoolType {} -> from AstT.BoolType
    Cst.IntType {} -> from AstT.IntType
    Cst.Arrow {start, remain} ->
      let newStart = cstToAstType start
          newRemain = cstToAstType <$> (snd <$> remain)
       in AstT.Arrow
            { start = newStart
            , remain = newRemain
            }
    Cst.Parens {_type} -> cstToAstType _type
    Cst.Variable {variableId} -> AstT.Variable {}


definitionParametersToParameters
  :: forall es
   . ( Reader SymbolResolutionState :> es
     , Error InferenceError :> es
     , State InferenceState :> es
     )
  => CstE.Parameters
  -> Eff
      es
      ( NonEmpty
          ( [Constraint]
          , ExpressionVariableId
          , AstT.Type InferenceVariable
          )
      )
definitionParametersToParameters params =
  mapM transParam (CstE.unParameters params)
  where
    transParam
      :: (CstE.Parameter, InfoId)
      -> Eff es ([Constraint], ExpressionVariableId, AstT.Type InferenceVariable)
    transParam (p, _) = do
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


definitionCstToAst
  :: ( Reader SymbolResolutionState :> es
     , Error InferenceError :> es
     , State InferenceState :> es
     , IdGenerator TypeVariableId :> es
     , Logger :> es
     )
  => CstE.Definition
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
  :: State InferenceState :> es
  => Error InferenceError :> es
  => Reader SymbolResolutionState :> es
  => IdGenerator TypeVariableId :> es
  => Logger :> es
  => CstE.Expression
  -> Eff es Output
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
          Output'
            { constraints = []
            , expression =
                from
                  AstE.VInt
                    { intValue = intValue
                    , inferType = from AstT.IntType
                    }
            }
      CstE.EBool {boolValue} -> do
        pure $
          Output'
            { constraints = []
            , expression =
                from
                  AstE.VBool
                    { boolValue = boolValue
                    , inferType = from AstT.BoolType
                    }
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
             in pure $
                  Output'
                    { constraints =
                        [ Constraint'
                            { constraintType1 = AstT.Variable eid
                            , constraintType2 = knowTy
                            , constraintInfo =
                                ConstraintInfo'
                                  { reason = IsKnowType
                                  , cst = from expr
                                  , ast = from newExp
                                  }
                            }
                        ]
                    , expression = newExp
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
                  from
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
          foldM go initialOut args
          where
            go preOut arg = do
              domain <- AstT.Variable <$> freshTypeVar
              codomain <- AstT.Variable <$> freshTypeVar
              outArgDomain <- check arg domain ArgumentShouldBeOfDomainType
              let
                preIsArrow =
                  Constraint'
                    { constraintType1 = AstE.getType preOut.expression
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
                )
      CstE.If {condition = cond, ifTrue, ifFalse, _then, _else} -> do
        condOut <- infer cond
        thenOut <- infer ifTrue
        elseOut <- infer ifFalse
        let
          newIf =
            AstE.If
              { condition = condOut.expression
              , ifTrue = thenOut.expression
              , ifFalse = elseOut.expression
              , inferType = AstE.getType thenOut.expression
              }
          conditionIsBool =
            Constraint'
              { constraintType1 = AstE.getType condOut.expression
              , constraintType2 = from AstT.BoolType
              , constraintInfo =
                  ConstraintInfo'
                    { reason = IfCasesShouldMatch
                    , cst = from cond
                    , ast = from condOut.expression
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
        pure $
          Output'
            { constraints =
                conditionIsBool
                  : thenIsElse
                  : ( condOut.constraints
                        <> elseOut.constraints
                        <> thenOut.constraints
                    )
            , expression = newIf
            }
      CstE.Let {definitions, expression = _in} -> do
        defs <- mapM definitionCstToAst (fst <$> definitions)
        inOut <- infer _in
        let
          newConstraints =
            concatMap fst (NonEmpty.toList defs)
              <> inOut.constraints
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
  inferLog (pretty @Text "Out:" <> format defaultFormatContext out)
  pure out


checkLog
  :: Logger :> es
  => Doc ann
  -> Eff es ()
checkLog d = debug (pretty @Text "Check:" <> d)


check
  :: State InferenceState :> es
  => Error InferenceError :> es
  => Reader SymbolResolutionState :> es
  => IdGenerator TypeVariableId :> es
  => Logger :> es
  => CstE.Expression
  -> AstT.Type InferenceVariable
  -> ConstraintReason
  -> Eff es Output
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
          Output'
            { constraints = []
            , expression =
                from
                  AstE.VInt
                    { intValue =
                        intValue
                    , inferType = from AstT.IntType
                    }
            }
      (CstE.EBool {boolValue}, AstT.VType AstT.BoolType) ->
        pure $
          Output'
            { constraints = []
            , expression =
                from
                  AstE.VBool
                    { boolValue =
                        boolValue
                    , inferType = from AstT.BoolType
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
