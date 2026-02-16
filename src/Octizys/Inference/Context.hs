{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use (,)" #-}
module Octizys.Inference.Context
  ( Context
  , emptyContext
  , contextFromList
  , lookup
  , addExpressionVars
  , addTypeToExpression
  , generalize
  ) where

import Data.Aeson (ToJSON)
import Data.Map (Map)
import qualified Data.Map as Map
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error)
import GHC.Generics (Generic, Generically (..))
import Octizys.Ast.Type (InferenceVariable)
import qualified Octizys.Ast.Type as Ast
import Octizys.Common.Format
import qualified Octizys.Common.Format as Common
import Octizys.Common.Id (ExpressionVariableId, TypeVariableId)
import Octizys.Effects.IdGenerator.Effect (IdGenerator, generateId)
import Prettyprinter (Pretty (pretty), concatWith, indent, line, (<+>))

import Control.Arrow ((<<<))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Octizys.Classes.FreeVariables (FreeVariables (freeVariables))
import Octizys.Classes.From (From (from))
import Prelude hiding (lookup)


data Context = Context'
  { expressionsMap :: Map ExpressionVariableId (Ast.Type InferenceVariable)
  , variablesInScope :: Set TypeVariableId
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically Context


instance Pretty Context where
  pretty (Context' c v) =
    let
      items = Map.toList c
      prettyItems =
        Common.prettyItemList items (pretty ',') (pretty ':')
     in
      pText "Context"
        <+> pretty '['
        <> line
        <> prettyItems
        <> line
        <> pretty ']'
        <+> pretty (Set.toList v)


instance FreeVariables InferenceVariable Context where
  freeVariables (Context' m _) =
    Set.unions ((freeVariables <<< snd) <$> Map.toList m)


emptyContext :: Context
emptyContext = Context' mempty mempty


contextFromList
  :: [(ExpressionVariableId, Ast.Type InferenceVariable)]
  -> [TypeVariableId]
  -> Context
contextFromList ls v =
  Context'
    ( Map.fromList
        ls
    )
    (Set.fromList v)


lookup
  :: Error Text :> es
  => Context
  -> ExpressionVariableId
  -> Eff es (Ast.Type InferenceVariable)
lookup context@(Context' ctx _) inferenceVar =
  case Map.lookup inferenceVar ctx of
    Just st -> pure st
    Nothing ->
      throwDocError
        ( pText "Can't find the required variable"
            <> indentPretty inferenceVar
            <> line
            <> pText "context"
            <> indentPretty context
        )


addExpressionVars
  :: Error Text :> es
  => [(ExpressionVariableId, Ast.Type InferenceVariable)]
  -> Context
  -> Eff es Context
addExpressionVars newVars (Context' ctx varScope) =
  let
    countUniques =
      Map.fromListWith
        (<>)
        ( (\(x, y) -> (x, [y]))
            <$> newVars
        )
    (regularItems, repeatedItems) =
      Map.mapEither
        ( \xs ->
            case xs of
              [x] -> Left x
              _ -> Right xs
        )
        countUniques
   in
    if Map.null repeatedItems
      then
        let inter = Map.intersectionWith (\x y -> (x, y)) ctx regularItems
         in if Map.null inter
              then pure $ Context' (Map.union ctx regularItems) varScope
              else
                let
                  errorMsg (name, (oldValue, newValue)) =
                    pText "The variable "
                      <> indentPretty name
                      <> line
                      <> pText "is already defined as"
                      <> indentPretty oldValue
                      <> line
                      <> pText "and is being redefined as "
                      <> indentPretty newValue
                 in
                  throwDocError
                    ( pText "Error"
                        <> indent
                          Common.defaultIndentationSpaces
                          ( concatWith
                              (\x y -> x <> line <> y)
                              ( errorMsg
                                  <$> Map.toList inter
                              )
                          )
                    )
      else
        throwDocError
          ( pText "Tried to add duplicate elements to context"
              <> indent
                Common.defaultIndentationSpaces
                ( line
                    <> pretty (Map.toList repeatedItems)
                )
          )


addTypeToExpression
  :: Error Text :> es
  => Context
  -> ExpressionVariableId
  -> Ast.Type InferenceVariable
  -> Eff es Context
addTypeToExpression context expVar ty = do
  addExpressionVars [(expVar, ty)] context


generalize
  :: IdGenerator TypeVariableId :> es
  => Context
  -> Ast.Type InferenceVariable
  -> Eff es (Ast.Type InferenceVariable)
generalize _ t@(Ast.TPoly _) = pure t
generalize c (Ast.TMono t) =
  let
    tFree :: Set InferenceVariable = freeVariables t
    contextFree :: Set InferenceVariable = freeVariables c
    binded :: Set InferenceVariable =
      Set.map
        Ast.RealTypeVariable
        c.variablesInScope
    toReplace = Set.difference (Set.union tFree contextFree) binded
   in
    do
      mapOfNewVars <-
        mapM
          ( \x -> do
              tyId <- generateId Nothing
              pure (x, Ast.Variable $ Ast.RealTypeVariable tyId)
          )
          (Set.toList toReplace)
      let
        replacementMap = Map.fromList mapOfNewVars
      pure (from $ Ast.replaceMonoVars replacementMap t)
