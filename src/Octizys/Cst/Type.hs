{- | Description : This module defines the `Type` used in the CST.
The `Type` is designed to be fault tolerant.
-}
module Octizys.Cst.Type
  ( Type
      ( BoolType
      , IntType
      , Arrow
      , Parens
      , TVariable
      )
  , info
  , start
  , remain
  , lparen
  , rparen
  , _type
  , variable
  , TypeVariableId
  , InfoId
  ) where

import Data.List.NonEmpty (NonEmpty)
import Octizys.Common.Id (InfoId, TypeVariableId)
import Octizys.Cst.SourceInfo (SourceInfo)


{- | Stores source information in a separate place, so
    we can mutate or do other things on it without
    modifying the tree.
-}
data Type tvar
  = -- | The boolean type hardcoded. Right now we don't have sum types or
    --  support for user defined types.
    BoolType {info :: SourceInfo}
  | -- | The int type hacoded. See `BoolType`
    IntType {info :: SourceInfo}
  | -- | Represent a function type.
    -- It can have multiple items, and it must have at least one.
    Arrow
      { start :: Type tvar
      , remain :: NonEmpty (SourceInfo, Type tvar)
      }
  | -- | Represents a type inside parentheses.
    Parens
      { lparen :: SourceInfo
      , _type :: Type tvar
      , rparen :: SourceInfo
      }
  | -- | All variables are translated at parsing time to a internal
    -- identifier. You can think of it as a pointer in symbol table.
    TVariable {info :: SourceInfo, variable :: tvar}
  deriving (Show, Eq, Ord)
