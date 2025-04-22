module Inference
  ( InferenceError,
    infer,
    InferenceExpressionVar (InferenceExpressionVarC),
    InferenceTypeVar (InferenceTypeVarC),
    InferenceExpression,
    InferenceType,
    check,
    InferenceContext,
  )
where

import Ast (Context, Expression, Type)

data InferenceError = InferenceError deriving (Show)

newtype InferenceTypeVar = InferenceTypeVarC Int

newtype InferenceExpressionVar = InferenceExpressionVarC Int

type InferenceExpression = Expression InferenceTypeVar InferenceExpressionVar

type InferenceType = Type InferenceTypeVar

type InferenceContext = Context InferenceExpressionVar InferenceTypeVar

infer ::
  InferenceContext ->
  InferenceExpression ->
  Either InferenceError InferenceType
infer = error "infer is unimplemented yet!"

check ::
  InferenceContext ->
  InferenceExpression ->
  InferenceType ->
  Either InferenceError ()
check = error "check is unimplemented yet!"
