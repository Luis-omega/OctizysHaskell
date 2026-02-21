module Octizys.Ast.Type.Basics
  ( TypeEq (..)
  , NormalizeType (..)
  , TypeValue (..)
  , InferenceVariable (..)
  , TypeVariable (..)
  , inferenceVarToId
  ) where

import Data.Set (singleton)
import Octizys.Classes.FreeVariables (FreeVariables (freeVariables))
import Octizys.Classes.From (From (from))

import Data.Aeson (ToJSON, ToJSONKey)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import GHC.Generics (Generic, Generically (..))
import Octizys.Common.Id (GenerateFromInt (generateFromInt), TypeVariableId)
import Octizys.Format.Class (Formattable (format))
import qualified Octizys.Format.Config as Format
import qualified Octizys.Format.Utils as Format
import Prettyprinter (Doc, Pretty, pretty)


-- * Classes


class TypeEq a where
  typeEq :: a -> a -> Bool


instance TypeEq a => TypeEq [a] where
  typeEq [] [] = True
  typeEq (x : xs) (y : ys) = typeEq x y && typeEq xs ys
  typeEq _ _ = False


instance TypeEq a => TypeEq (NonEmpty a) where
  typeEq x y = typeEq (NonEmpty.toList x) (NonEmpty.toList y)


class NormalizeType a where
  normalize :: a -> a


-- * Type Values


data TypeValue = BoolType | IntType
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically TypeValue


instance FreeVariables TypeVariableId TypeValue where
  freeVariables _ = mempty


instance Pretty TypeValue where
  pretty BoolType = pretty @Text "Bool"
  pretty IntType = pretty @Text "Int"


instance Formattable TypeValue where
  format = formatValue


instance TypeEq TypeValue where
  typeEq = (==)


-- * Inference Variables


data InferenceVariable
  = ErrorVariable Text
  | RealTypeVariable TypeVariableId
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically InferenceVariable


instance ToJSONKey InferenceVariable


instance FreeVariables TypeVariableId InferenceVariable where
  freeVariables (RealTypeVariable tid) = singleton tid
  freeVariables (ErrorVariable _) = mempty


instance Pretty InferenceVariable where
  pretty (ErrorVariable _) = pretty @Text "ErrorVariable"
  pretty (RealTypeVariable vid) = pretty vid


instance Formattable InferenceVariable where
  format _ = pretty


instance From InferenceVariable TypeVariableId where
  from = RealTypeVariable


instance GenerateFromInt InferenceVariable where
  generateFromInt sc oi i = RealTypeVariable $ generateFromInt sc oi i


instance TypeEq InferenceVariable where
  typeEq (RealTypeVariable x) (RealTypeVariable y) = x == y
  typeEq _ _ = False


-- * Type Variables


newtype TypeVariable = TypeVariable' {unTypeVariable :: TypeVariableId}
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically TypeVariable


instance From TypeVariable TypeVariableId where
  from = TypeVariable'


instance From TypeVariableId TypeVariable where
  from = unTypeVariable


instance Pretty TypeVariable where
  pretty (TypeVariable' vid) = pretty vid


instance Formattable TypeVariable where
  format _ = pretty


instance FreeVariables TypeVariableId TypeVariable where
  freeVariables v = singleton v.unTypeVariable


instance GenerateFromInt TypeVariable where
  generateFromInt sc oi i = TypeVariable' $ generateFromInt sc oi i


instance TypeEq TypeVariable where
  typeEq = (==)


-- * Auxiliary functions


inferenceVarToId :: InferenceVariable -> Maybe TypeVariableId
inferenceVarToId (RealTypeVariable i) = pure i
inferenceVarToId (ErrorVariable _) = Nothing


formatValue :: Format.Configuration -> TypeValue -> Doc ann
formatValue _ BoolType = Format.text "Bool"
formatValue _ IntType = Format.text "Int"
