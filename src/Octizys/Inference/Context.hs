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
import qualified Octizys.Ast.Type as Ast
import Octizys.Common.Id (ExpressionVariableId, TypeVariableId)
import Octizys.Effects.IdGenerator.Effect (IdGenerator, generateId)
import Prettyprinter (Pretty (pretty), concatWith, line, (<+>))

import Control.Arrow ((<<<))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Octizys.Ast.Type.Basics (InferenceVariable)
import qualified Octizys.Ast.Type.Basics as Ast
import qualified Octizys.Ast.Type.MonoType as Ast
import Octizys.Classes.FreeVariables (FreeVariables (freeVariables))
import Octizys.Classes.From (From (from))
import Octizys.Format.Class (Formattable (format))
import Octizys.Format.Config (defaultConfiguration)
import qualified Octizys.Format.Utils as Format
import qualified Prettyprinter as Pretty
import Prelude hiding (lookup)


data Context = Context'
  { expressionsMap :: Map ExpressionVariableId (Ast.Type InferenceVariable)
  , variablesInScope :: Set TypeVariableId
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically Context


instance Formattable Context where
  format configuration (Context' c v) =
    let
      prettyItems =
        Format.formatMapWith
          configuration
          (\con (k, y) -> format con k <> pretty ':' <> format con y)
          Pretty.comma
          c
     in
      Format.text "Context"
        <> prettyItems
        <+> Format.formatSet configuration v


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
      Format.throwDocError
        ( Format.text "Can't find the required variable"
            <> Format.indentFormat defaultConfiguration inferenceVar
            <> line
            <> Format.text "context"
            <> Format.indentFormat defaultConfiguration context
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
                    Format.text "The variable "
                      <> Format.indentFormat defaultConfiguration name
                      <> line
                      <> Format.text "is already defined as"
                      <> Format.indentFormat defaultConfiguration oldValue
                      <> line
                      <> Format.text "and is being redefined as "
                      <> Format.indentFormat defaultConfiguration newValue
                 in
                  Format.throwDocError
                    ( Format.text "Error"
                        <> Format.indentDoc
                          defaultConfiguration
                          ( concatWith
                              (\x y -> x <> line <> y)
                              ( errorMsg
                                  <$> Map.toList inter
                              )
                          )
                    )
      else
        Format.throwDocError
          ( Format.text "Tried to add duplicate elements to context"
              <> Format.indentDoc
                defaultConfiguration
                ( line
                    <> Format.formatMapWith
                      defaultConfiguration
                      ( \c -> Format.formatTupleItemsWith c format Format.formatList (pretty ':')
                      )
                      Pretty.comma
                      repeatedItems
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
              pure (x, Ast.MonoVariable $ Ast.RealTypeVariable tyId)
          )
          (Set.toList toReplace)
      let
        replacementMap = Map.fromList mapOfNewVars
      pure (from $ Ast.replaceVars replacementMap t)
