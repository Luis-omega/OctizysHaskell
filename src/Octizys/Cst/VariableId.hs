{- | This module defines a Id type to be used in
all elements of Cst and Ast whenever a reference to a variable
is necesarie.

This allow us to have a symbol table that can be mutated
without touching the CST.
-}
module Octizys.Cst.VariableId
  ( VariableId (VariableId')
  , unVariableId
  ) where

import Octizys.Effects.Generator.Interpreter (GenerateFromInt)
import Prettyprinter (Pretty)


{- | A id that point to the variable information stored in a
symbol table.
-}
newtype VariableId = VariableId' {unVariableId :: Int}
  deriving
    ( Show
    , Eq
    , Ord
    , GenerateFromInt
    , Pretty
    )
    via Int
