{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Octizys.Evaluation
  ( EvaluationError
  , evaluateExpression
  , EvaluationExpressionVar (EvaluationExpressionVarC)
  , EvaluationExpression
  )
where

import Effectful (Eff, (:>))
import Effectful.Error.Dynamic (Error, throwError)
import Octizys.Ast (Expression)


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
