{-# LANGUAGE MultiParamTypeClasses #-}

module Octizys.Ast.Expression where

import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (difference)
import Data.Text (Text)
import Effectful.Dispatch.Dynamic (HasCallStack)
import Octizys.Ast.Type (Type)
import Octizys.Classes.FreeVariables (FreeTypeVariables (freeTyVars))
import Octizys.Classes.From (From (from))
import Octizys.Cst.Expression (ExpressionVariableId)


data Definition = Definition'
  { name :: ExpressionVariableId
  , definition :: Expression
  , inferType :: Type
  }
  deriving (Show, Eq, Ord)


instance FreeTypeVariables Definition where
  freeTyVars d =
    freeTyVars d.definition <> freeTyVars d.inferType


data Value
  = VInt {intValue :: Text, inferType :: Type}
  | VBool {boolValue :: Bool, inferType :: Type}
  | Function
      { parameters :: NonEmpty (ExpressionVariableId, Type)
      , body :: Expression
      , inferType :: Type
      }
  deriving (Show, Eq, Ord)


getValueType :: Value -> Type
getValueType v = v.inferType


instance FreeTypeVariables Value where
  freeTyVars (VInt {inferType}) = freeTyVars inferType
  freeTyVars (VBool {inferType}) = freeTyVars inferType
  freeTyVars (Function {inferType, body, parameters}) =
    difference
      (freeTyVars inferType <> freeTyVars body)
      (foldl' (\a (_, y) -> a <> freeTyVars y) mempty parameters)


data Expression
  = Variable {name :: ExpressionVariableId, inferType :: Type}
  | EValue {value :: Value, inferType :: Type}
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


instance From Expression Value where
  from v = EValue {value = v, inferType = getValueType v}


buildValueDefinitionsMap :: Value -> Map ExpressionVariableId Expression
buildValueDefinitionsMap VInt {} = mempty
buildValueDefinitionsMap VBool {} = mempty
buildValueDefinitionsMap Function {body} = buildDefinitionsMap body


buildDefinitionsMap :: Expression -> Map ExpressionVariableId Expression
buildDefinitionsMap Variable {} = mempty
buildDefinitionsMap EValue {value} = buildValueDefinitionsMap value
buildDefinitionsMap Application {applicationFunction, applicationArgument} =
  Map.union
    (buildDefinitionsMap applicationFunction)
    (buildDefinitionsMap applicationArgument)
buildDefinitionsMap If {condition, ifTrue, ifFalse} =
  Map.union
    (buildDefinitionsMap condition)
    ( Map.union
        (buildDefinitionsMap ifTrue)
        (buildDefinitionsMap ifFalse)
    )
buildDefinitionsMap Let {definitions, expression} =
  Map.union
    (buildDefinitionsMap expression)
    (foldMap buildFromDefinition definitions)
  where
    buildFromDefinition :: Definition -> Map ExpressionVariableId Expression
    buildFromDefinition Definition' {name, definition} =
      Map.union (Map.singleton name definition) (buildDefinitionsMap definition)
buildDefinitionsMap Annotation {expression} =
  buildDefinitionsMap expression


instance FreeTypeVariables Expression where
  freeTyVars (EValue {inferType}) = freeTyVars inferType
  freeTyVars (Variable {inferType}) = freeTyVars inferType
  freeTyVars (Application {inferType, applicationFunction, applicationArgument}) =
    freeTyVars inferType
      <> freeTyVars applicationFunction
      <> freeTyVars applicationArgument
  freeTyVars (If {inferType, condition, ifTrue, ifFalse}) =
    freeTyVars inferType
      <> freeTyVars ifTrue
      <> freeTyVars ifFalse
      <> freeTyVars condition
  freeTyVars (Let {inferType, definitions, expression}) =
    freeTyVars inferType
      <> freeTyVars expression
      <> foldMap freeTyVars definitions
  freeTyVars Annotation {expression, _type, inferType} =
    freeTyVars expression <> freeTyVars _type <> freeTyVars inferType


getType :: HasCallStack => Expression -> Type
getType e = e.inferType
