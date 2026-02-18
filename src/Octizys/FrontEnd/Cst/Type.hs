{- | Description : This module defines the `Type` used in the CST.
The `Type` is designed to be fault tolerant.
-}
module Octizys.FrontEnd.Cst.Type
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
  ) where

import Data.List.NonEmpty (NonEmpty, cons)
import Octizys.Common.Id (TypeVariableId)
import Octizys.FrontEnd.Cst.SourceInfo (SourceInfo)

import Data.Aeson (ToJSON)
import GHC.Generics (Generic, Generically (..))
import Octizys.Format.Class (Formattable (format))
import qualified Octizys.Format.Config as Format
import qualified Octizys.Format.Utils as Format
import Prettyprinter (Doc)
import qualified Prettyprinter as Pretty


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
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (Type tvar)


instance Formattable tvar => Formattable (Type tvar) where
  format = formatType


-- * Format


-- | To determine if the arrow arguments needs parens or not.
needsParentsInArrow :: Type tvar -> Bool
needsParentsInArrow t =
  case t of
    IntType {} -> False
    BoolType {} -> False
    Arrow {} -> True
    Parens {} -> True
    TVariable {} -> False


formatType
  :: forall tvar ann
   . Formattable tvar
  => Format.Configuration
  -> Type tvar
  -> Doc ann
formatType configuration t =
  case t of
    IntType _ -> Format.text "Int"
    BoolType _ -> Format.text "Bool"
    Arrow {start = _domain, remain = _codomain} ->
      Format.formatListItemsWith
        configuration
        prettyArg
        (Format.text "->")
        (cons _domain (snd <$> _codomain))
      where
        prettyArg c ty =
          if needsParentsInArrow ty
            then Pretty.parens (formatType c ty)
            else formatType c ty
    Parens {_type = t2} ->
      formatType configuration t2
    TVariable {variable = v} -> format configuration v
