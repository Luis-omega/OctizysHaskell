module Octizys.Ast.Type where

import Data.List.NonEmpty (NonEmpty)
import Octizys.Cst.Type (TypeVariableId)


data Type
  = -- | The boolean type hardcoded. Right now we don't have sum types or
    --  support for user defined types.
    BoolType
  | -- | The int type hacoded. See `BoolType`
    IntType
  | -- | Represent a function type.
    -- It can have multiple items, and it must have at least one.
    Arrow
      { start :: Type
      , remain :: NonEmpty Type
      }
  | -- | Represents a type inside parentheses.
    Parens
      { _type :: Type
      }
  | -- | All variables are translated at parsing time to a internal
    -- identifier. You can think of it as a pointer in symbol table.
    Variable {variableId :: TypeVariableId}
  deriving (Show, Eq, Ord)
