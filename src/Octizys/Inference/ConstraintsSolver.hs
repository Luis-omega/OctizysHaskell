{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Octizys.Inference.ConstraintsSolver where

import qualified Octizys.Cst.Expression as CstE

import Control.Monad (foldM, forM, unless)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Local (State)
import qualified Octizys.Ast.Expression as AstE
import qualified Octizys.Ast.Type as AstT
import Octizys.Classes.FreeVariables (FreeTypeVariables (freeTyVars))
import Octizys.Classes.From (From (from))
import Octizys.Cst.Type
  ( TypeVariableId
  )
import Octizys.Effects.Generator.Effect (Generator)
import Octizys.Effects.Logger.Effect (Logger, debug)
import Octizys.Effects.SymbolResolution.Interpreter (SymbolResolutionState)
import Octizys.Inference.ConstraintsGeneration
  ( InferenceState
  , Output (constraints, expression)
  , definitionCstToAst
  , infer
  )
import Octizys.Inference.Errors
  ( Constraint (constraintType1, constraintType2)
  , InferenceError
    ( CantUnify
    , ContainsFreeVariablesAfterSolving
    , RecursiveSubstitution
    )
  , Substitution (Substitution', substitutionMap)
  , singletonSubstitution
  )
import Octizys.Pretty.FormatContext (defaultFormatContext)
import Octizys.Pretty.Formatter (Formatter (format))
import Prettyprinter (Doc, Pretty (pretty), (<+>))
import qualified Prettyprinter as Pretty
import Prelude hiding (lookup)


{- | Check if a variable is in the substitution, if it is
then we check that the variable doesn't pear in it's value
before returning the right value.
-}
occursCheck
  :: Error InferenceError :> es
  => TypeVariableId
  -> Substitution
  -> Eff es (Maybe (Either InferenceError AstT.Type))
occursCheck tid s =
  case Map.lookup tid s.substitutionMap of
    Just value ->
      case value of
        Left e -> pure $ Just $ Left e
        Right mp ->
          if Set.member tid (freeTyVars mp)
            then throwError $ RecursiveSubstitution s
            else pure $ Just $ Right mp
    Nothing -> pure Nothing


-- | Applies a substitution of terms to a type.
subs
  :: Error InferenceError :> es
  => Substitution
  -> AstT.Type
  -> Eff es (Either InferenceError AstT.Type)
subs sub oldType =
  case oldType of
    AstT.VType AstT.BoolType -> pure oldType
    AstT.VType AstT.IntType -> pure oldType
    AstT.Arrow {start, remain} -> do
      newStart <- subs sub start
      newRemain <- forM remain (subs sub)
      pure $
        Right $
          AstT.Arrow
            { start = newStart
            , remain = newRemain
            }
    AstT.Variable {variableId = oldVid} -> do
      maybeValue <- occursCheck oldVid sub
      case maybeValue of
        Just newValue -> pure newValue
        Nothing -> pure oldType


-- | Apply a substitution to a constraint.
subsInConstraint
  :: Error InferenceError :> es
  => Substitution
  -> Constraint
  -> Eff es Constraint
subsInConstraint sub c = do
  newT1 <- subs sub c.constraintType1
  newT2 <- subs sub c.constraintType2
  pure
    c
      { constraintType1 = newT1
      , constraintType2 = newT2
      }


composeSubstitutions
  :: Error InferenceError :> es
  => Substitution
  -> Substitution
  -> Eff es Substitution
composeSubstitutions s1 s2 = do
  substituted <- traverse step s2.substitutionMap
  pure $ Substitution' (Map.union substituted s1.substitutionMap)
  where
    step (Left e) = pure (Left e)
    step (Right b) = Right <$> subs s1 b


unify
  :: Error InferenceError :> es
  => AstT.Type
  -> AstT.Type
  -> Eff es Substitution
unify x y =
  if x == y
    then pure mempty
    else case (x, y) of
      (AstT.Arrow {remain = remainX}, AstT.Arrow {remain = remainY}) -> do
        startU <- unify x.start y.start
        args <- go remainX remainY
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
      (AstT.Variable tid, _) -> do
        pure $ singletonSubstitution tid y
      (_, AstT.Variable _) -> unify y x
      _ -> throwError $ CantUnify x y


defSubs
  :: Error InferenceError :> es
  => Substitution
  -> AstE.Definition
  -> Eff es AstE.Definition
defSubs s d@(AstE.Definition' {definition, inferType}) = do
  newDef <- expSubs s definition
  newType <- subs s inferType
  pure
    d
      { AstE.definition = newDef
      , AstE.inferType = newType
      }


expSubs
  :: Error InferenceError :> es
  => Substitution
  -> AstE.Expression
  -> Eff es AstE.Expression
expSubs s e =
  case e of
    AstE.EValue {value = AstE.VInt {inferType, intValue}, inferType = ty} -> do
      newType <- subs s inferType
      newTypeTy <- subs s ty
      pure $
        AstE.EValue
          { value = AstE.VInt {intValue, AstE.inferType = newType}
          , AstE.inferType = newTypeTy
          }
    AstE.EValue {value = AstE.VBool {inferType, boolValue}, inferType = ty} -> do
      newType <- subs s inferType
      newTypeTy <- subs s ty
      pure $
        AstE.EValue
          { value = AstE.VBool {boolValue, AstE.inferType = newType}
          , AstE.inferType = newTypeTy
          }
    AstE.Variable {inferType} -> do
      newType <- subs s inferType
      pure $ e {AstE.inferType = newType}
    AstE.EValue
      { value =
        AstE.Function
          { parameters
          , body
          , inferType
          }
      } -> do
        newType <- subs s inferType
        newParams <- forM parameters (\(x, y) -> subs s y >>= \v -> pure (x, v))
        newBody <-
          expSubs s body
        pure $
          from
            AstE.Function
              { AstE.inferType = newType
              , AstE.parameters = newParams
              , AstE.body = newBody
              }
    AstE.Application
      { applicationFunction
      , applicationArgument
      , inferType
      } -> do
        newType <- subs s inferType
        newFunction <-
          expSubs s applicationFunction
        newArg <-
          expSubs s applicationArgument
        pure $
          e
            { AstE.inferType = newType
            , AstE.applicationFunction = newFunction
            , AstE.applicationArgument = newArg
            }
    AstE.If {inferType, condition, ifTrue, ifFalse} -> do
      newType <- subs s inferType
      newCondition <-
        expSubs s condition
      newTrue <-
        expSubs s ifTrue
      newFalse <-
        expSubs s ifFalse
      pure $
        e
          { AstE.inferType = newType
          , AstE.condition = newCondition
          , AstE.ifTrue = newTrue
          , AstE.ifFalse = newFalse
          }
    AstE.Let {inferType, definitions, expression} -> do
      newType <- subs s inferType
      newDef <-
        forM definitions (defSubs s)
      newExpr <-
        expSubs s expression
      pure $
        e
          { AstE.inferType = newType
          , AstE.definitions = newDef
          , AstE.expression = newExpr
          }
    AstE.Annotation {expression, _type, inferType} -> do
      newType <- subs s inferType
      newType2 <- subs s _type
      newExpr <-
        expSubs s expression
      pure $
        e
          { AstE.inferType = newType
          , AstE._type = newType2
          , AstE.expression = newExpr
          }


constraintSubstitution
  :: Error InferenceError :> es
  => Constraint
  -> Eff es Substitution
constraintSubstitution c =
  unify c.constraintType1 c.constraintType2


findSubstitution
  :: Error InferenceError :> es
  => [Constraint]
  -> Eff es Substitution
findSubstitution cs = do
  substitutions <- forM cs constraintSubstitution
  foldM composeSubstitutions mempty substitutions


logSolver
  :: Logger :> es
  => Text
  -> Doc ann
  -> Eff es ()
logSolver header msg = debug (pretty @Text "Solver:" <> pretty header <+> Pretty.nest 2 msg)


solveExpressionType
  :: State InferenceState :> es
  => Error InferenceError :> es
  => Reader SymbolResolutionState :> es
  => Generator TypeVariableId :> es
  => Logger :> es
  => CstE.Expression
  -> Eff es AstE.Expression
solveExpressionType expression = do
  logSolver "Got:" $ format defaultFormatContext expression
  out <- infer expression
  logSolver "constraints:" $
    Pretty.list (format defaultFormatContext <$> out.constraints)
  logSolver "afterInfer:" $ format defaultFormatContext out.expression
  subst <- findSubstitution out.constraints
  logSolver "substitutions:" $
    Pretty.list
      (format defaultFormatContext <$> Map.toList subst.substitutionMap)
  final <- expSubs subst out.expression
  let free = freeTyVars final
  unless
    (Set.null free)
    ( throwError $
        ContainsFreeVariablesAfterSolving
          (from expression)
          (from final)
          free
          subst
    )
  pure final


solveDefinitionType
  :: State InferenceState :> es
  => Error InferenceError :> es
  => Reader SymbolResolutionState :> es
  => Generator TypeVariableId :> es
  => Logger :> es
  => CstE.Definition
  -> Eff es AstE.Definition
solveDefinitionType cstDef = do
  (constraints, astDef) <- definitionCstToAst cstDef
  subst <- findSubstitution constraints
  final <- defSubs subst astDef
  let free = freeTyVars final
  unless
    (Set.null free)
    ( throwError $
        ContainsFreeVariablesAfterSolving
          (from cstDef)
          (from final)
          free
          subst
    )
  pure final


definitionsCstToAst
  :: State InferenceState :> es
  => Reader SymbolResolutionState :> es
  => Generator TypeVariableId :> es
  => Logger :> es
  => [CstE.Definition]
  -> Eff
      es
      ( Either
          [InferenceError]
          [(CstE.Definition, AstE.Definition, [Constraint])]
      )
definitionsCstToAst cstDefs = go cstDefs [] []
  where
    go [] accOk accErr =
      case accErr of
        [] -> pure (Right accOk)
        _ -> pure $ Left accErr
    go (cst : xs) accOk accErr = do
      maybeResult <- runErrorNoCallStack (definitionCstToAst cst)
      case maybeResult of
        Left i -> go xs accOk (i : accErr)
        Right (cs, ast) -> go xs ((cst, ast, cs) : accOk) accErr


solveDefinitionsType
  :: State InferenceState :> es
  => Reader SymbolResolutionState :> es
  => Generator TypeVariableId :> es
  => Logger :> es
  => [CstE.Definition]
  -> Eff es ([InferenceError], [AstE.Definition])
solveDefinitionsType = undefined

-- solveDefinitionsType cstDef = do
--   maybeDefs <- definitionsCstToAst cstDef
--   case maybeDefs of
--     Left ls -> pure (ls,[])
--     Right rsts ->
--       let
--         constraints =
--           concatMap (\(_, _, cs) -> cs) rsts
--        in
--         do
--           subst <- findSubstitutions constraints
--           final <- forM astDefs (defSubs subst)
--           let free = foldl' (<>) mempty (freeTyVars <$> final)
--           unless
--             (Set.null free)
--             ( throwError $
--                 ContainsFreeVariablesAfterSolving
--                   (from cstDef)
--                   (from final)
--                   free
--                   subst
--             )
--           pure final
