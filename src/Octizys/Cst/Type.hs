{- | Description : This module defines the `Type` used in the CST.
The `Type` is designed to be fault tolerant.
-}
module Octizys.Cst.Type
  ( Type (BoolType, IntType, Arrow, Parens, Variable)
  , info
  , start
  , remain
  , lparen
  , rparen
  , _type
  , variableId
  , TypeVariableId (TypeVariableId')
  , freshTypeVariableId
  ) where

import Data.List.NonEmpty (NonEmpty)
import Effectful (Eff, (:>))
import Octizys.Cst.InfoId (InfoId)
import Octizys.Cst.VariableId (VariableId)
import Octizys.Effects.Generator (GenerateFromInt, Generator, generate)


-- | A wrapper around VariableId to represent type variables.
newtype TypeVariableId
  = TypeVariableId' VariableId
  deriving
    ( Show
    , Eq
    , GenerateFromInt
    , Ord
    )
    via VariableId


freshTypeVariableId
  :: Generator TypeVariableId :> es
  => Eff es TypeVariableId
freshTypeVariableId = generate


{- | `Type` has multiple design choices inside it.

  * To be fault tolerant
  * To store source information in a separate place, so
    we can mutate or do other things on it without
    modifying the tree.
  * To be used as part of "Parsing Concrete Tree", "Inference Tree", "Export tree"
-}
data Type
  = -- | The boolean type hardcoded. Right now we don't have sum types or
    --  support for user defined types.
    BoolType {info :: InfoId}
  | -- | The int type hacoded. See `BoolType`
    IntType {info :: InfoId}
  | -- | Represent a function type.
    -- It can have multiple items, and it must have at least one.
    Arrow
      { info :: InfoId
      , start :: Type
      , remain :: NonEmpty Type
      }
  | -- | Represents a type inside parentheses.
    Parens
      { info :: InfoId
      , lparen :: InfoId
      , _type :: Type
      , rparen :: InfoId
      }
  | -- | All variables are translated at parsing time to a internal
    -- identifier. You can think of it as a pointer in symbol table.
    Variable {info :: InfoId, variableId :: TypeVariableId}
  deriving (Show, Eq, Ord)
