{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Evaluation (EvaluationError, evaluateExpression) where

import Ast (Context, Expression)
import Effectful (Eff, (:>))
import Effectful.Error.Dynamic (Error, throwError)

data EvaluationError = UndefinedEvaluation deriving (Show)

evaluateExpression :: (Error EvaluationError :> es) => Context -> Expression -> Eff es (Expression, Context)
evaluateExpression _ _ = throwError UndefinedEvaluation
