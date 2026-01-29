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
import Octizys.Classes.FreeVariables (FreeVariables (freeVariables))
import Octizys.Classes.From (From (from))
import Octizys.Common.Id (ExpressionVariableId, TypeVariableId)

import Data.Aeson (ToJSON)
import GHC.Generics (Generic, Generically (..))


data Definition var = Definition'
  { name :: ExpressionVariableId
  , definition :: Expression var
  , inferType :: Type var
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (Definition var)


instance
  FreeVariables TypeVariableId var
  => FreeVariables TypeVariableId (Definition var)
  where
  freeVariables d =
    freeVariables d.definition <> freeVariables d.inferType


data Value var
  = VInt {intValue :: Text, inferType :: Type var}
  | VBool {boolValue :: Bool, inferType :: Type var}
  | Function
      { parameters :: NonEmpty (ExpressionVariableId, Type var)
      , body :: Expression var
      , inferType :: Type var
      }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (Value var)


getValueType :: Value var -> Type var
getValueType v = v.inferType


instance
  FreeVariables TypeVariableId var
  => FreeVariables TypeVariableId (Value var)
  where
  freeVariables (VInt {inferType}) = freeVariables inferType
  freeVariables (VBool {inferType}) = freeVariables inferType
  freeVariables (Function {inferType, body, parameters}) =
    difference
      (freeVariables inferType <> freeVariables body)
      (foldl' (\a (_, y) -> a <> freeVariables y) mempty parameters)


data Expression var
  = Variable {name :: ExpressionVariableId, inferType :: Type var}
  | EValue {value :: Value var, inferType :: Type var}
  | Application
      { applicationFunction :: Expression var
      , applicationArgument :: Expression var
      , inferType :: Type var
      }
  | If
      { condition :: Expression var
      , ifTrue :: Expression var
      , ifFalse :: Expression var
      , inferType :: Type var
      }
  | Let
      { -- The alone info is the semicolon finishing a definition
        definitions :: NonEmpty (Definition var)
      , expression :: Expression var
      , inferType :: Type var
      }
  | Annotation
      { expression :: Expression var
      , _type :: Type var
      , inferType :: Type var
      }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (Expression var)


instance From (Expression var) (Value var) where
  from v = EValue {value = v, inferType = getValueType v}


buildValueDefinitionsMap
  :: Value var -> Map ExpressionVariableId (Expression var)
buildValueDefinitionsMap VInt {} = mempty
buildValueDefinitionsMap VBool {} = mempty
buildValueDefinitionsMap Function {body} = buildDefinitionsMap body


buildDefinitionsMap
  :: Expression var -> Map ExpressionVariableId (Expression var)
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
    buildFromDefinition
      :: Definition var -> Map ExpressionVariableId (Expression var)
    buildFromDefinition Definition' {name, definition} =
      Map.union (Map.singleton name definition) (buildDefinitionsMap definition)
buildDefinitionsMap Annotation {expression} =
  buildDefinitionsMap expression


instance
  FreeVariables TypeVariableId var
  => FreeVariables TypeVariableId (Expression var)
  where
  freeVariables (EValue {inferType}) = freeVariables inferType
  freeVariables (Variable {inferType}) = freeVariables inferType
  freeVariables (Application {inferType, applicationFunction, applicationArgument}) =
    freeVariables inferType
      <> freeVariables applicationFunction
      <> freeVariables applicationArgument
  freeVariables (If {inferType, condition, ifTrue, ifFalse}) =
    freeVariables inferType
      <> freeVariables ifTrue
      <> freeVariables ifFalse
      <> freeVariables condition
  freeVariables (Let {inferType, definitions, expression}) =
    freeVariables inferType
      <> freeVariables expression
      <> foldMap freeVariables definitions
  freeVariables Annotation {expression, _type, inferType} =
    freeVariables expression <> freeVariables _type <> freeVariables inferType


getType :: HasCallStack => Expression var -> Type var
getType e = e.inferType
