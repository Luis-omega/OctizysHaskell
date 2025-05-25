module Octizys.Classes.FreeVariables where

import Data.Set (Set)
import Octizys.Cst.Expression (ExpressionVariableId)
import Octizys.Cst.Type (TypeVariableId)


class FreeExpressionVariables t where
  freeExpVars :: t -> Set ExpressionVariableId


class FreeTypeVariables t where
  freeTyVars :: t -> Set TypeVariableId
