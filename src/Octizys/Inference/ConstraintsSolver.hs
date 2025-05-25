{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Octizys.Inference.ConstraintsSolver where

import qualified Octizys.Cst.Expression as CstE

import Control.Arrow ((<<<))
import Control.Monad (foldM, forM, unless)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Set as Set
import Data.Text (Text)
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.State.Static.Local (State)
import qualified Octizys.Ast.Expression as AstE
import qualified Octizys.Ast.Type as AstT
import Octizys.Classes.FreeVariables (FreeTypeVariables (freeTyVars))
import Octizys.Classes.From (From (from))
import Octizys.Cst.Type
  ( TypeVariableId
  )
import Octizys.Effects.Logger.Effect (Logger, debug)
import Octizys.Inference.ConstraintsGeneration
  ( InferenceState
  , Output (constraints, expression)
  , definitionCstToAst
  , infer
  )
import Octizys.Inference.Errors
  ( Constraint (constraintInfo, constraintType1, constraintType2)
  , InferenceError
    ( CantUnify
    , ContainsFreeVariablesAfterSolving
    , RecursiveSubstitution
    )
  , Substitution (Substitution', substitutionInfo, value, variableId)
  )
import Octizys.Pretty.FormatContext (defaultFormatContext)
import Octizys.Pretty.Formatter (Formatter (format))
import Prettyprinter (Doc, Pretty (pretty), (<+>))
import qualified Prettyprinter as Pretty
import Prelude hiding (lookup)


-- | Replaces a type variable inside a expression with another type.
subs
  :: Error InferenceError :> es
  => Substitution
  -> AstT.Type
  -> Eff es AstT.Type
subs sub oldType =
  let freeInType = freeTyVars sub.value
   in if Set.member sub.variableId freeInType
        then throwError $ RecursiveSubstitution sub
        else case oldType of
          AstT.VType AstT.BoolType -> pure oldType
          AstT.VType AstT.IntType -> pure oldType
          AstT.Arrow {start, remain} -> do
            newStart <- subs sub start
            newRemain <- forM remain (subs sub)
            pure $
              AstT.Arrow
                { start = newStart
                , remain = newRemain
                }
          AstT.Variable {variableId = oldVid} ->
            pure $
              if oldVid == sub.variableId
                then sub.value
                else oldType


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
              newVars = Set.unions (map (freeTyVars <<< value) related)
              newCurrent = Set.union newVars newSeen
           in go newCurrent newSeen (acc ++ related)

    partitionSubstitutions
      :: Set.Set TypeVariableId -> [Substitution] -> ([Substitution], [Substitution])
    partitionSubstitutions vars = foldr step ([], [])
      where
        step s (yes, no)
          | s.variableId `Set.member` vars
              || not (Set.null (Set.intersection (freeTyVars (value s)) vars)) =
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
            forM
              ys
              ( subsInConstraint $
                  constraintToSub t x
              )
              >>= findSubstitutions
          pure
            ( constraintToSub t x
                : lst
            )
        _ ->
          case x.constraintType2 of
            AstT.Variable t -> do
              lst <-
                forM
                  ys
                  ( subsInConstraint
                      ( constraintToSub
                          t
                          ( x {constraintType1 = x.constraintType2, constraintType2 = x.constraintType1}
                          )
                      )
                  )
                  >>= findSubstitutions
              pure
                ( constraintToSub
                    t
                    ( x {constraintType1 = x.constraintType2, constraintType2 = x.constraintType1}
                    )
                    : lst
                )
            _ -> do
              unifySubs <- unify x
              findSubstitutions (unifySubs <> ys)
    subsSub s s2 = do
      newVal <- subs s s2.value
      pure s2 {value = newVal}
    subsInInitial [] end = pure end
    subsInInitial (z : zs) ss =
      forM ss (subsSub z) >>= subsInInitial zs
    constraintToSub :: TypeVariableId -> Constraint -> Substitution
    constraintToSub t c =
      Substitution'
        { variableId = t
        , value = c.constraintType2
        , substitutionInfo = c.constraintInfo
        }


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
    (flip subs)
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
    AstE.EValue {value = AstE.VInt {inferType, intValue}, inferType = ty} -> do
      newType <- typeSubs l inferType
      newTypeTy <- typeSubs l ty
      pure $
        AstE.EValue
          { value = AstE.VInt {intValue, AstE.inferType = newType}
          , AstE.inferType = newTypeTy
          }
    AstE.EValue {value = AstE.VBool {inferType, boolValue}, inferType = ty} -> do
      newType <- typeSubs l inferType
      newTypeTy <- typeSubs l ty
      pure $
        AstE.EValue
          { value = AstE.VBool {boolValue, AstE.inferType = newType}
          , AstE.inferType = newTypeTy
          }
    AstE.Variable {inferType} -> do
      newType <- typeSubs l inferType
      pure $ e {AstE.inferType = newType}
    AstE.EValue
      { value =
        AstE.Function
          { parameters
          , body
          , inferType
          }
      } -> do
        newType <- typeSubs l inferType
        newParams <- forM parameters (\(x, y) -> typeSubs l y >>= \v -> pure (x, v))
        newBody <-
          expSubs l body
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
  logSolver "Got:" $ format defaultFormatContext expression
  out <- infer expression
  logSolver "constraints:" $
    Pretty.list (format defaultFormatContext <$> out.constraints)
  logSolver "afterInfer:" $ format defaultFormatContext out.expression
  subst <- findSubstitutions out.constraints
  logSolver "substitutions:" $
    Pretty.list (format defaultFormatContext <$> subst)
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
  => Logger :> es
  => CstE.Definition
  -> Eff es AstE.Definition
solveDefinitionType cstDef = do
  (constraints, astDef) <- definitionCstToAst cstDef
  subst <- findSubstitutions constraints
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
