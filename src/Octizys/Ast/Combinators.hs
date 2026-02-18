module Octizys.Ast.Combinators where

import Data.IORef (IORef)
import qualified Data.List.NonEmpty as NonEmpty
import Octizys.Ast.Type
  ( InferenceVariable
  , MonoType (..)
  , Scheme (Scheme')
  , Type
  , TypeValue (..)
  , TypeVariable
  )
import Octizys.Classes.From (From (from))
import Octizys.Common.Id (TypeVariableId)
import qualified Octizys.FrontEnd.Cst.Combinators as Cst


boolType :: MonoType tvar
boolType = from BoolType


intType :: MonoType tvar
intType = from IntType


tVar :: IORef Int -> IO (MonoType InferenceVariable)
tVar counter = Variable <$> Cst.makeVariable counter


arrow :: [MonoType tvar] -> MonoType tvar -> MonoType tvar
arrow [] t = t
arrow (x : xs) out =
  Arrow
    x
    ( NonEmpty.prependList
        xs
        (out NonEmpty.:| [])
    )


scheme :: [TypeVariableId] -> MonoType tvar -> Type tvar
scheme [] body = from body
scheme (x : xs) body = from $ Scheme' (x NonEmpty.:| xs) body
