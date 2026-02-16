{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Octizys.Ast.Node where

import Octizys.Ast.Expression (Definition, Expression, Value)
import Octizys.Ast.Type (Type)
import Octizys.Classes.From (From (from))
import Octizys.Common.Id (ExpressionVariableId)

import Data.Aeson (ToJSON)
import GHC.Generics (Generic, Generically (..))
import Prettyprinter (Pretty (pretty))


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


instance Pretty var => Pretty (Node var) where
  pretty (NType t) = pretty t
  pretty (NExp t) = pretty t
  pretty (NDef t) = pretty t
  pretty (NParameter t) = pretty t
  pretty (NValue t) = pretty t
