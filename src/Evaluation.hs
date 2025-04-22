{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Evaluation
  ( EvaluationError
  , evaluateExpression
  , EvaluationExpressionVar (EvaluationExpressionVarC)
  , EvaluationExpression
  )
where

import Ast (Expression)
import Effectful (Eff, (:>))
import Effectful.Error.Dynamic (Error, throwError)


data EvaluationError = UndefinedEvaluation deriving (Show)


newtype EvaluationExpressionVar = EvaluationExpressionVarC Int
  deriving (Show)


type EvaluationTypeVar = ()


type EvaluationExpression =
  Expression EvaluationExpressionVar EvaluationTypeVar


evaluateExpression
  :: Error EvaluationError :> es
  => ()
  -> EvaluationExpression
  -> Eff es (EvaluationExpression, ())
evaluateExpression _ _ = throwError UndefinedEvaluation
