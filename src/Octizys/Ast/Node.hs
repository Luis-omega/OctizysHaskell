{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Octizys.Ast.Node where

import Octizys.Ast.Expression (Definition, Expression, Value)
import Octizys.Ast.Type (Type)
import Octizys.Classes.From (From (from))
import Octizys.Cst.Expression (ExpressionVariableId)


data Node
  = NType Type
  | NExp Expression
  | NDef Definition
  | NParameter (ExpressionVariableId, Type)
  | NValue Value
  deriving (Show, Eq, Ord)


instance From Node Type where
  from = NType


instance From Node Expression where
  from = NExp


instance From Node Definition where
  from = NDef


instance From Node (ExpressionVariableId, Type) where
  from = NParameter


instance From Node Value where
  from = NValue
