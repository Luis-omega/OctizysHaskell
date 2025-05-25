{-# LANGUAGE MultiParamTypeClasses #-}

module Octizys.Ast.Type where

import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty)
import Data.Set (singleton)
import Octizys.Classes.FreeVariables (FreeTypeVariables (freeTyVars))
import Octizys.Classes.From (From (from))
import Octizys.Cst.Type (TypeVariableId)


data TypeValue = BoolType | IntType
  deriving (Show, Eq, Ord)


data Type
  = VType {value :: TypeValue}
  | -- | Represent a function type.
    -- It can have multiple items, and it must have at least one.
    Arrow
      { start :: Type
      , remain :: NonEmpty Type
      }
  | -- | All variables are translated at parsing time to a internal
    -- identifier. You can think of it as a pointer in symbol table.
    Variable {variableId :: TypeVariableId}
  deriving (Show, Eq, Ord)


instance From Type TypeValue where
  from = VType


instance From Type TypeVariableId where
  from = Variable


instance FreeTypeVariables Type where
  freeTyVars (Arrow {start, remain}) =
    foldl'
      (<>)
      (freeTyVars start)
      (freeTyVars <$> remain)
  freeTyVars (Variable d) = singleton d
  freeTyVars _ = mempty
