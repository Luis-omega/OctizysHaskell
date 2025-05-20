module Octizys.Ast.Type where

import Control.Arrow ((<<<))
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty, cons, toList)
import Data.Text (Text)
import Octizys.Cst.Type (TypeVariableId)
import Prettyprinter (Pretty (pretty))
import qualified Prettyprinter as Pretty


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
  | -- | All variables are translated at parsing time to a internal
    -- identifier. You can think of it as a pointer in symbol table.
    Variable {variableId :: TypeVariableId}
  deriving (Show, Eq, Ord)


needsParentsInArrow :: Type -> Bool
needsParentsInArrow t =
  case t of
    IntType {} -> False
    BoolType {} -> False
    Arrow {} -> True
    Variable {} -> False


instance Pretty Type where
  pretty BoolType = "Bool"
  pretty IntType = "Int"
  pretty Arrow {start, remain} =
    (Pretty.group <<< Pretty.nest 2)
      ( Pretty.line'
          <> Pretty.concatWith
            (\l r -> l <> Pretty.line <> pretty @Text "->" <> r)
            ( prettyArg
                <$> cons start remain
            )
      )
    where
      prettyArg ty =
        if needsParentsInArrow ty
          then Pretty.parens (pretty ty)
          else pretty ty
  pretty Variable {variableId = v} = pretty '_' <> pretty v
