{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Octizys.Ast.Node where

import Octizys.Ast.Expression (Definition, Expression, Value)
import Octizys.Ast.Type (Type)
import Octizys.Classes.From (From (from))
import Octizys.Common.Id (ExpressionVariableId)


data Node var
  = NType (Type var)
  | NExp (Expression var)
  | NDef (Definition var)
  | NParameter (ExpressionVariableId, Type var)
  | NValue (Value var)
  deriving (Show, Eq, Ord)


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
