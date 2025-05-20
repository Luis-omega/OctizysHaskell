module Octizys.Ast.Expression where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Effectful.Dispatch.Dynamic (HasCallStack)
import Octizys.Ast.Type (Type)
import Octizys.Cst.Expression (ExpressionVariableId)


data Definition = Definition'
  { name :: ExpressionVariableId
  , definition :: Expression
  , inferType :: Type
  }
  deriving (Show, Eq, Ord)


data Expression
  = EInt {intValue :: Text, inferType :: Type}
  | EBool {boolValue :: Bool, inferType :: Type}
  | Variable {name :: ExpressionVariableId, inferType :: Type}
  | Function
      { parameters :: NonEmpty (ExpressionVariableId, Type)
      , body :: Expression
      , inferType :: Type
      }
  | Application
      { applicationFunction :: Expression
      , applicationArgument :: Expression
      , inferType :: Type
      }
  | If
      { condition :: Expression
      , ifTrue :: Expression
      , ifFalse :: Expression
      , inferType :: Type
      }
  | Let
      { -- The alone info is the semicolon finishing a definition
        definitions :: NonEmpty Definition
      , expression :: Expression
      , inferType :: Type
      }
  | Annotation
      { expression :: Expression
      , _type :: Type
      , inferType :: Type
      }
  deriving (Show, Eq, Ord)


getType :: HasCallStack => Expression -> Type
getType e = e.inferType
