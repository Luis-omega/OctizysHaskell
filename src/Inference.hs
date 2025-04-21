module Inference (InferenceError, infer, check) where

import Ast (Context, Expression, Type)


data InferenceError = InferenceError deriving (Show)


infer :: Context -> Expression -> Either Type InferenceError
infer = error "infer is unimplemented yet!"


check :: Context -> Expression -> Type -> Either Type InferenceError
check = error "check is unimplemented yet!"
