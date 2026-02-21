{- | Description : This module defines the `Type` used in the CST.
The `Type` is designed to be fault tolerant.
-}
module Octizys.FrontEnd.Cst.Type
  ( Type
      ( TBool
      , TInt
      , TArrow
      , TParens
      , TVariable
      )
  , BoolType (BoolType', info)
  , IntType (IntType', info)
  , Arrow (Arrow', start, remain)
  , Parens (Parens', lparen, _type, rparen)
  , Variable (Variable', info, variable)
  ) where

import Data.List.NonEmpty (NonEmpty, cons)
import Octizys.FrontEnd.Cst.SourceInfo (SourceInfo)

import Data.Aeson (ToJSON)
import GHC.Generics (Generic, Generically (..))
import Octizys.Classes.From (From, from)
import Octizys.Format.Class (Formattable (format))
import qualified Octizys.Format.Config as Format
import qualified Octizys.Format.Utils as Format
import Prettyprinter (Doc)
import qualified Prettyprinter as Pretty


newtype BoolType = BoolType' {info :: SourceInfo}
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically BoolType


instance Formattable BoolType where
  format _ _ = Format.text "Bool"


newtype IntType = IntType' {info :: SourceInfo}
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically IntType


instance Formattable IntType where
  format _ _ = Format.text "Int"


data Arrow tvar = Arrow'
  { start :: Type tvar
  , remain :: NonEmpty (SourceInfo, Type tvar)
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (Arrow tvar)


instance Formattable tvar => Formattable (Arrow tvar) where
  format = formatArrow


data Parens tvar = Parens'
  { lparen :: SourceInfo
  , _type :: Type tvar
  , rparen :: SourceInfo
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (Parens tvar)


instance Formattable tvar => Formattable (Parens tvar) where
  format = formatParens


data Variable tvar = Variable' {info :: SourceInfo, variable :: tvar}
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (Variable tvar)


instance Formattable tvar => Formattable (Variable tvar) where
  format = formatVariable


data Type tvar
  = -- | The boolean type hardcoded. Right now we don't have sum types or
    --  support for user defined types.
    TBool BoolType
  | -- | The int type hacoded. See `BoolType`
    TInt IntType
  | -- | Represent a function type.
    -- It can have multiple items, and it must have at least one.
    TArrow (Arrow tvar)
  | -- | Represents a type inside parentheses.
    TParens (Parens tvar)
  | TVariable (Variable tvar)
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (Type tvar)


instance Formattable tvar => Formattable (Type tvar) where
  format = formatType


instance From (Type tvar) BoolType where
  from = TBool


instance From (Type tvar) IntType where
  from = TInt


instance From (Type tvar) (Arrow tvar) where
  from = TArrow


instance From (Type tvar) (Parens tvar) where
  from = TParens


instance From (Type tvar) (Variable tvar) where
  from = TVariable


-- * Format


-- | To determine if the arrow arguments needs parens or not.
needsParentsInArrow :: Type tvar -> Bool
needsParentsInArrow t =
  case t of
    TInt {} -> False
    TBool {} -> False
    TArrow {} -> True
    TParens {} -> True
    TVariable {} -> False


formatArrow
  :: Formattable tvar => Format.Configuration -> Arrow tvar -> Doc ann
formatArrow configuration (Arrow' {start = _domain, remain = _codomain}) =
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


formatParens
  :: Formattable tvar => Format.Configuration -> Parens tvar -> Doc ann
formatParens configuration (Parens' {_type}) =
  Pretty.parens (formatType configuration _type)


formatVariable
  :: Formattable a => Format.Configuration -> Variable a -> Doc ann
formatVariable configuration (Variable' {variable}) =
  format configuration variable


formatType
  :: forall tvar ann
   . Formattable tvar
  => Format.Configuration
  -> Type tvar
  -> Doc ann
formatType configuration t =
  case t of
    TInt v -> format configuration v
    TBool v -> format configuration v
    TArrow a -> format configuration a
    TParens v -> formatParens configuration v
    TVariable v -> formatVariable configuration v
