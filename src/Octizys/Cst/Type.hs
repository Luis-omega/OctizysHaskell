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
  , TypeVariableId
    ( TypeVariableId'
    , unTypeVariableId
    )
  ) where

import Data.List.NonEmpty (NonEmpty, last)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import Octizys.Cst.InfoId
  ( HasInfoSpan (getInfoSpan)
  , InfoId
  , InfoSpan (OneInfo, TwoInfo)
  , infoSpanStart
  )
import Octizys.Cst.VariableId (VariableId)
import Octizys.Effects.Generator.Interpreter (GenerateFromInt)
import Prettyprinter (Pretty (pretty))


-- | A wrapper around VariableId to represent type variables.
newtype TypeVariableId = TypeVariableId' {unTypeVariableId :: VariableId}
  deriving
    ( Eq
    , GenerateFromInt
    , Ord
    )
    via VariableId
  deriving (Show)


instance Pretty TypeVariableId where
  pretty (TypeVariableId' i) = pretty @Text "_t" <> pretty i


{- | `Type` has multiple design choices inside it.

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
      { start :: Type
      , remain :: NonEmpty (InfoId, Type)
      }
  | -- | Represents a type inside parentheses.
    Parens
      { lparen :: InfoId
      , _type :: Type
      , rparen :: InfoId
      }
  | -- | All variables are translated at parsing time to a internal
    -- identifier. You can think of it as a pointer in symbol table.
    Variable {info :: InfoId, variableId :: TypeVariableId}
  deriving (Show, Eq, Ord)


instance HasInfoSpan Type where
  getInfoSpan BoolType {info} = OneInfo info
  getInfoSpan IntType {info} = OneInfo info
  getInfoSpan Arrow {start, remain} =
    getInfoSpan start <> getInfoSpan (snd (NonEmpty.last remain))
  getInfoSpan Parens {lparen, rparen} = TwoInfo lparen rparen
  getInfoSpan Variable {info} = OneInfo info
