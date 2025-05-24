module Octizys.Ast.Evaluation where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Debug.Trace (trace)
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Octizys.Ast.Expression
import Octizys.Ast.Type (Type (Arrow, BoolType, remain, start))
import Octizys.Cst.Expression (ExpressionVariableId)
import Octizys.Effects.Logger.Effect (Logger, debug, errorLog, info)
import Prettyprinter (Pretty (pretty))
import qualified Prettyprinter as Pretty
import Text.Show.Pretty (ppShow)


data EvaluationError
  = UnknowExpressionVar
      ExpressionVariableId
      (Map ExpressionVariableId Expression)
  | InvalidArgumentApplication Expression
  deriving (Show)


instance Pretty EvaluationError where
  pretty (UnknowExpressionVar e mp) =
    pretty @Text "Unknown expression id : "
      <> pretty e
      <> ", this is a bug, please report it."
      <> Pretty.line
      <> Pretty.list (pretty <$> Map.toList mp)
  pretty (InvalidArgumentApplication e) =
    pretty @Text "Attempt to apply a non function: "
      <> pretty e


substituteInDef
  :: ExpressionVariableId
  -> Expression
  -> Definition
  -> Definition
substituteInDef vId e def =
  def {definition = substitute vId e def.definition}


{- | Substitution of a expression variable for a value
inside another expression.
-}
substitute
  :: ExpressionVariableId
  -- Replace the variable for this
  -> Expression
  -- Replace the variable in this expression
  -> Expression
  -> Expression
substitute vId replacementExp e =
  case e of
    EInt {} -> e
    EBool {} -> e
    Variable {name} -> if name == vId then replacementExp else e
    Function {body} ->
      e {body = substitute vId replacementExp body}
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
  => Map ExpressionVariableId Expression
  -> Expression
  -> Eff es Expression
evaluateExpression _ x@EInt {} = pure x
evaluateExpression _ x@EBool {} = pure x
evaluateExpression context e@Variable {name} =
  case Map.lookup name context of
    Just value ->
      evaluateExpression context value
    Nothing -> pure e
evaluateExpression _ e@Function {} = pure e
evaluateExpression
  context
  e@Application
    { applicationArgument
    , applicationFunction
    } =
    do
      evalLog "Evaluating f:" (prettyExpression pretty applicationFunction)
      f <- evaluateExpression context applicationFunction
      evalLog "Evaluating arg:" (prettyExpression pretty applicationArgument)
      arg <- evaluateExpression context applicationArgument
      case f of
        Function
          { parameters = (param, _) :| otherParams
          , body
          , inferType = Arrow {remain}
          } -> do
            evalLog
              "Substituting variable: "
              ( pretty param
                  <> " as "
                  <> pretty arg
                  <> pretty @Text " in "
                  <> prettyExpression pretty body
              )
            application <- evaluateExpression context (substitute param arg body)
            evalLog
              "Application of function got :"
              (prettyExpression pretty application)
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
                    Function {parameters = x :| rest, body = application, inferType = newType}
        _ -> throwError $ InvalidArgumentApplication e
evaluateExpression context If {condition, ifTrue, ifFalse} = do
  c <- evaluateExpression context condition
  if c == EBool {boolValue = True, inferType = BoolType}
    then evaluateExpression context ifTrue
    else evaluateExpression context ifFalse
evaluateExpression context Let {expression} = do
  evalLog
    "Evaluating let inner expression :"
    (prettyExpression pretty expression)
  evaluateExpression
    context
    expression
evaluateExpression context Annotation {expression} = evaluateExpression context expression
