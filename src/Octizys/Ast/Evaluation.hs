{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Octizys.Ast.Evaluation where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Octizys.Ast.Expression
import Octizys.Ast.Type
  ( MonoType (Arrow, VType, remain, start)
  , Type (TMono)
  , TypeValue (BoolType)
  , TypeVariable
  )
import Octizys.Classes.From (From (from))
import Octizys.Common.Id (ExpressionVariableId)
import Octizys.Effects.Logger.Effect (Logger, debug)
import Octizys.Pretty.FormatContext
  ( FormatContext
  , defaultFormatContext
  , formatExpressionVar
  )
import Octizys.Pretty.Formatter (Formatter, format)
import Prettyprinter (Pretty (pretty))
import qualified Prettyprinter as Pretty


data EvaluationError
  = UnknowExpressionVar
      ExpressionVariableId
      (Map ExpressionVariableId (Expression TypeVariable))
  | InvalidArgumentApplication (Expression TypeVariable)
  deriving (Show)


instance Formatter ann (FormatContext ann) EvaluationError where
  format ctx (UnknowExpressionVar e mp) =
    pretty @Text "Unknown expression id : "
      <> formatExpressionVar ctx e
      <> ", this is a bug, please report it."
      <> Pretty.line
      <> Pretty.list
        ( ( \(x, y) ->
              Pretty.parens
                ( formatExpressionVar ctx x
                    <> pretty ','
                    <> format ctx y
                )
          )
            <$> Map.toList mp
        )
  format ctx (InvalidArgumentApplication e) =
    pretty @Text "Attempt to apply a non function: "
      <> format ctx e


substituteInDef
  :: ExpressionVariableId
  -> Expression TypeVariable
  -> Definition TypeVariable
  -> Definition TypeVariable
substituteInDef vId e def =
  def {definition = substitute vId e def.definition}


substituteInValue
  :: ExpressionVariableId
  -> Expression TypeVariable
  -> Value TypeVariable
  -> Value TypeVariable
substituteInValue vId e v =
  case v of
    VInt {} -> v
    VBool {} -> v
    -- As we have all variables as unique, this should
    -- be fine, i gues...
    -- TODO: TESTS THIS!
    Function {body} -> v {body = substitute vId e body}


{- | Substitution of a expression variable for a value
inside another expression.
-}
substitute
  :: ExpressionVariableId
  -- Replace the variable for this
  -> Expression TypeVariable
  -- Replace the variable in this expression
  -> Expression TypeVariable
  -> Expression TypeVariable
substitute vId replacementExp e =
  case e of
    EValue {value} -> e {value = substituteInValue vId replacementExp value}
    Variable {name} -> if name == vId then replacementExp else e
    Application {applicationFunction, applicationArgument} ->
      let newF = substitute vId replacementExp applicationFunction
          newArg = substitute vId replacementExp applicationArgument
       in e
            { applicationFunction = newF
            , applicationArgument = newArg
            }
    If {condition, ifTrue, ifFalse} ->
      let newCond = substitute vId replacementExp condition
          newTrue = substitute vId replacementExp ifTrue
          newFalse = substitute vId replacementExp ifFalse
       in e {condition = newCond, ifTrue = newTrue, ifFalse = newFalse}
    Let {definitions, expression} ->
      let newDefs = substituteInDef vId replacementExp <$> definitions
          newExp = substitute vId replacementExp expression
       in e {definitions = newDefs, expression = newExp}
    Annotation {expression} ->
      e {expression = substitute vId replacementExp expression}


evalLog
  :: Logger :> es
  => Text
  -> Pretty.Doc ann
  -> Eff es ()
evalLog header msg = debug (pretty header <> msg)


evaluateExpression
  :: Error EvaluationError :> es
  => Logger :> es
  => Map ExpressionVariableId (Expression TypeVariable)
  -> Expression TypeVariable
  -> Eff es (Value TypeVariable)
evaluateExpression _ EValue {value} = pure value
evaluateExpression context Variable {name} =
  case Map.lookup name context of
    Just value ->
      evaluateExpression context value
    Nothing -> throwError $ UnknowExpressionVar name context
evaluateExpression
  context
  e@Application
    { applicationArgument
    , applicationFunction
    } =
    do
      evalLog "Evaluating f:" (format defaultFormatContext applicationFunction)
      f <- evaluateExpression context applicationFunction
      evalLog "Evaluating arg:" (format defaultFormatContext applicationArgument)
      arg <- evaluateExpression context applicationArgument
      case f of
        Function
          { parameters = (param, _) :| otherParams
          , body
          , inferType = TMono (Arrow {remain})
          } -> do
            evalLog
              "Substituting variable: "
              ( formatExpressionVar defaultFormatContext param
                  <> " as "
                  <> format defaultFormatContext arg
                  <> pretty @Text " in "
                  <> format defaultFormatContext body
              )
            application <- evaluateExpression context (substitute param (from arg) body)
            evalLog
              "Application of function got :"
              (format defaultFormatContext application)
            let
              newType = case remain of
                lst :| [] -> lst
                new :| other : others ->
                  Arrow {start = new, remain = other :| others}
             in
              case otherParams of
                [] -> pure application
                (x : rest) ->
                  pure
                    Function
                      { parameters = x :| rest
                      , body = from application
                      , inferType = from newType
                      }
        _ -> throwError $ InvalidArgumentApplication e
evaluateExpression context If {condition, ifTrue, ifFalse} = do
  c <- evaluateExpression context condition
  if c
    == VBool
      { boolValue =
          True
      , inferType = from $ VType @TypeVariable BoolType
      }
    then evaluateExpression context ifTrue
    else evaluateExpression context ifFalse
evaluateExpression context Let {expression} = do
  evalLog
    "Evaluating let inner expression :"
    (format defaultFormatContext expression)
  evaluateExpression
    context
    expression
evaluateExpression context Annotation {expression} = evaluateExpression context expression
