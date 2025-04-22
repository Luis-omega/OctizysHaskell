{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Evaluation
  ( EvaluationError
  , evaluateExpression
  , EvaluationExpressionVar (EvaluationExpressionVarC)
  , EvaluationExpression
  , EvaluationContext
  )
where

import Ast (Context, Expression)
import Effectful (Eff, (:>))
import Effectful.Error.Dynamic (Error, throwError)


data EvaluationError = UndefinedEvaluation deriving (Show)


newtype EvaluationExpressionVar = EvaluationExpressionVarC Int
  deriving (Show)


type EvaluationTypeVar = ()


type EvaluationExpression =
  Expression EvaluationExpressionVar EvaluationTypeVar


type EvaluationContext = Context EvaluationExpressionVar EvaluationTypeVar


evaluateExpression
  :: Error EvaluationError :> es
  => EvaluationContext
  -> EvaluationExpression
  -> Eff es (EvaluationExpression, EvaluationContext)
evaluateExpression _ _ = throwError UndefinedEvaluation
