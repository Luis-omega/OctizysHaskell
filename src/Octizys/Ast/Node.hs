{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Octizys.Ast.Node where

import Octizys.Ast.Expression (Definition, Expression, Value)
import Octizys.Ast.Type (Type)
import Octizys.Classes.From (From (from))
import Octizys.Common.Id (ExpressionVariableId)

import Data.Aeson (ToJSON)
import GHC.Generics (Generic, Generically (..))
import Octizys.Format.Class (Formattable (format))
import qualified Octizys.Format.Utils as Format
import qualified Prettyprinter as Pretty


data Node var
  = NType (Type var)
  | NExp (Expression var)
  | NDef (Definition var)
  | NParameter (ExpressionVariableId, Type var)
  | NValue (Value var)
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (Node var)


instance From (Node var) (Type var) where
  from = NType


instance From (Node var) (Expression var) where
  from = NExp


instance From (Node var) (Definition var) where
  from = NDef


instance From (Node var) (ExpressionVariableId, Type var) where
  from = NParameter


instance From (Node var) (Value var) where
  from = NValue


instance Formattable var => Formattable (Node var) where
  format configuration (NType t) = format configuration t
  format configuration (NExp t) = format configuration t
  format configuration (NDef t) = format configuration t
  format configuration (NParameter t) =
    Format.formatTupleItems
      configuration
      Pretty.comma
      t
  format configuration (NValue t) = format configuration t
