module Octizys.Ast.Expression.Value where

import Control.Arrow ((<<<))
import Data.Aeson (ToJSON)
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (difference)
import Data.Text (Text)
import GHC.Generics (Generic, Generically (..))
import Octizys.Ast.Type.MonoType (MonoType)

import Effectful.Dispatch.Dynamic (HasCallStack)
import Prettyprinter (Doc, Pretty (pretty), (<+>))
import qualified Prettyprinter as Pretty

import Octizys.Ast.Type (Type)
import Octizys.Classes.FreeVariables (FreeVariables (freeVariables))
import Octizys.Classes.From (From (from))
import Octizys.Common.Id (ExpressionVariableId, TypeVariableId)
import Octizys.Format.Class (Formattable, format)
import qualified Octizys.Format.Config as Format
import qualified Octizys.Format.Utils as Format


data ValueInt var = ValueInt'
  { intValue :: Text
  , inferType :: MonoType var
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (ValueInt var)


data ValueBool var = ValueBool'
  { boolValue :: Bool
  , inferType :: MonoType var
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (ValueBool var)
