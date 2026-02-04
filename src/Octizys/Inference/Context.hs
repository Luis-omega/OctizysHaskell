{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use (,)" #-}
module Octizys.Inference.Context where

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
import Octizys.Common.Id (ExpressionVariableId)
import Prettyprinter (Pretty (pretty), concatWith, indent, line, (<+>))


newtype Context = Context'
  { expressionsMap :: Map ExpressionVariableId (Ast.Type InferenceVariable)
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically Context


instance Pretty Context where
  pretty (Context' c) =
    let
      items = Map.toList c
      prettyItems =
        Common.prettyItemList items (pretty ',') (pretty ':')
     in
      pretty @String "Context"
        <+> pretty '['
        <> line
        <> prettyItems
        <> line
        <> pretty ']'


emptyContext :: Context
emptyContext = Context' mempty


contextFromList
  :: [(ExpressionVariableId, Ast.Type InferenceVariable)]
  -> Context
contextFromList ls =
  Context'
    ( Map.fromList
        ls
    )


findExpressionVariable
  :: Error String :> es
  => ExpressionVariableId
  -> Context
  -> Eff es (Ast.Type InferenceVariable)
findExpressionVariable inferenceVar context@(Context' ctx) =
  case Map.lookup inferenceVar ctx of
    Just st -> pure st
    Nothing ->
      throwDocError
        ( pString "Can't find the required variable"
            <> indentPretty inferenceVar
            <> line
            <> pString "context"
            <> indentPretty context
        )


addExpressionVars
  :: Error String :> es
  => [(ExpressionVariableId, Ast.Type InferenceVariable)]
  -> Context
  -> Eff es Context
addExpressionVars newVars (Context' ctx) =
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
              then pure $ Context' $ Map.union ctx regularItems
              else
                let
                  errorMsg (name, (oldValue, newValue)) =
                    pString "The variable "
                      <> indentPretty name
                      <> line
                      <> pString "is already defined as"
                      <> indentPretty oldValue
                      <> line
                      <> pString "and is being redefined as "
                      <> indentPretty newValue
                 in
                  throwDocError
                    ( pString "Error"
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
          ( pString "Tried to add duplicate elements to context"
              <> indent
                Common.defaultIndentationSpaces
                ( line
                    <> pretty (Map.toList repeatedItems)
                )
          )


addTypeToExpression
  :: Context
  -> ExpressionVariableId
  -> Ast.Type InferenceVariable
  -> Context
addTypeToExpression context inferenceVar ty =
  Context' $ Map.insert inferenceVar ty context.expressionsMap
