module Octizys.Scope where

import Data.Map (Map)
import Octizys.Ast.Expression (Expression)
import Octizys.Ast.Type (Type, TypeVariable)
import Octizys.Common.Id (ExpressionVariableId)
import Octizys.Cst.Comment (Comment)
import Octizys.Cst.Span (Span)


data ExpressionVariableInformation = ExpressionVariableInformation'
  { documentation :: [Comment]
  -- ^ Any documentation comment in the orinal source.
  , span :: Span
  -- ^ The original source @Span@ of the definition.
  , _type :: Type TypeVariable
  -- ^ Types!
  , ast :: Maybe (Expression TypeVariable)
  -- ^ ast is available iff the variable is a definition and the ast
  -- corresponds to the body of the definition (after desugarization).
  }


newtype ImportsScope = ImportsScope'
  { imports :: Map ExpressionVariableId ExpressionVariableInformation
  }


data ModuleScope = ModuleScope' {}


data LocalScope = LocalScope' {}
