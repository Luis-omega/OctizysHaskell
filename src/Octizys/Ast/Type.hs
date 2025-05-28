{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Octizys.Ast.Type where

import Control.Arrow ((<<<))
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Set (singleton)
import qualified Data.Set as Set
import Octizys.Classes.FreeVariables (FreeVariables (freeVariables))
import Octizys.Classes.From (From (from))
import Octizys.Cst.Type (TypeVariableId)


data TypeValue = BoolType | IntType
  deriving (Show, Eq, Ord)


instance FreeVariables TypeVariableId TypeValue where
  freeVariables _ = mempty


data InferenceVariable
  = ErrorVariable
  | MetaVariable TypeVariableId
  deriving (Show, Eq, Ord)


instance FreeVariables TypeVariableId InferenceVariable where
  freeVariables (MetaVariable tid) = singleton tid
  freeVariables ErrorVariable = mempty


newtype TypeVariable = TypeVariable {unTypeVariable :: TypeVariableId}
  deriving (Show, Eq, Ord)


instance FreeVariables TypeVariableId TypeVariable where
  freeVariables v = singleton v.unTypeVariable


data MonoType var
  = VType {value :: TypeValue}
  | -- | Represent a function type.
    -- It can have multiple items, and it must have at least one.
    Arrow
      { start :: MonoType var
      , remain :: NonEmpty (MonoType var)
      }
  | -- | All variables are translated at parsing time to a internal
    -- identifier. You can think of it as a pointer in symbol table.
    Variable var
  deriving (Show, Eq, Ord)


instance From (MonoType var) TypeValue where
  from = VType


instance
  FreeVariables TypeVariableId var
  => FreeVariables TypeVariableId (MonoType var)
  where
  freeVariables VType {value} = freeVariables value
  freeVariables Arrow {start, remain} =
    foldl'
      (<>)
      (freeVariables start)
      (freeVariables <$> remain)
  freeVariables (Variable v) = freeVariables v


data Scheme var = Scheme'
  { arguments :: NonEmpty TypeVariableId
  , body :: MonoType var
  }
  deriving (Show, Eq, Ord)


instance
  FreeVariables TypeVariableId var
  => FreeVariables TypeVariableId (Scheme var)
  where
  freeVariables s =
    Set.difference
      (freeVariables s.body)
      (Set.fromList (NonEmpty.toList s.arguments))


data Type var
  = TMono (MonoType var)
  | TPoly (Scheme var)
  deriving (Show, Eq, Ord)


instance From (Type var) TypeValue where
  from = TMono <<< from


instance From (Type var) (MonoType var) where
  from = TMono


instance From (Type var) (Scheme var) where
  from = TPoly


instance
  FreeVariables TypeVariableId var
  => FreeVariables TypeVariableId (Type var)
  where
  freeVariables (TMono m) = freeVariables m
  freeVariables (TPoly s) = freeVariables s
