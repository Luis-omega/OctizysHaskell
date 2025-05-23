module Octizys.Ast.Evaluation where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Effectful (Eff, (:>))
import Effectful.Error.Dynamic (Error, throwError)
import Effectful.State.Static.Local (State, gets)
import Octizys.Ast.Expression
import Octizys.Ast.Type (Type (BoolType))
import Octizys.Cst.Expression (ExpressionVariableId)
import Prettyprinter (Pretty (pretty))


data EvaluationError = UnknowExpressionVar ExpressionVariableId
  deriving (Show)


instance Pretty EvaluationError where
  pretty (UnknowExpressionVar e) =
    pretty @Text "Unknow expression id : "
      <> pretty e
      <> ", this is a bug, please report it."


newtype EvaluationState = EvaluationState'
  { varToExp :: Map ExpressionVariableId Expression
  }


initialEvaluationState :: EvaluationState
initialEvaluationState =
  EvaluationState'
    { varToExp = mempty
    }


evaluateExpression
  :: Error EvaluationError :> es
  => State EvaluationState :> es
  => Expression
  -> Eff es Expression
evaluateExpression x@EInt {} = pure x
evaluateExpression x@EBool {} = pure x
evaluateExpression Variable {name} = do
  evst <- gets varToExp
  case Map.lookup name evst of
    Just v -> evaluateExpression v
    Nothing -> throwError $ UnknowExpressionVar name
evaluateExpression e@Function {} = pure e
evaluateExpression
  Application
    { applicationArgument
    , applicationFunction
    } =
    do
      f <- evaluateExpression applicationFunction
      arg <- evaluateExpression applicationArgument
      undefined
evaluateExpression If {condition, ifTrue, ifFalse} = do
  c <- evaluateExpression condition
  if c == EBool {boolValue = True, inferType = BoolType}
    then evaluateExpression ifTrue
    else evaluateExpression ifFalse
-- TODO: FIXME
evaluateExpression Let {expression} =
  evaluateExpression expression
evaluateExpression Annotation {expression} = evaluateExpression expression
