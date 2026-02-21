module Octizys.Scope where

import Data.Map (Map)
import EffectfulParserCombinators.Span (Span)
import Octizys.Ast.Expression (Expression)
import Octizys.Ast.Type (Type)
import Octizys.Ast.Type.Basics (TypeVariable)
import Octizys.Common.Id (ExpressionVariableId)
import Octizys.FrontEnd.Cst.Comment (Comment)


data ExpressionVariableInformation = ExpressionVariableInformation'
  { documentation :: [Comment]
  -- ^ Any documentation comment in the original source.
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
