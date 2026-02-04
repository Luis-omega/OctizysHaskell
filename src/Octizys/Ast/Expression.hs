{-# LANGUAGE MultiParamTypeClasses #-}

module Octizys.Ast.Expression where

import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (difference)
import Data.Text (Text)
import Effectful.Dispatch.Dynamic (HasCallStack)
import Octizys.Ast.Type (MonoType, Type)
import Octizys.Classes.FreeVariables (FreeVariables (freeVariables))
import Octizys.Classes.From (From (from))
import Octizys.Common.Id (ExpressionVariableId, TypeVariableId)

import Control.Arrow ((<<<))
import Data.Aeson (ToJSON)
import GHC.Generics (Generic, Generically (..))
import Octizys.Common.Format (defaultIndentationSpaces)
import Octizys.Common.Format.Config (formatText)
import Prettyprinter (Doc, Pretty (pretty), (<+>))
import qualified Prettyprinter as Pretty


data Definition var = Definition'
  { name :: ExpressionVariableId
  , definition :: Expression var
  , inferType :: Type var
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (Definition var)


instance Pretty var => Pretty (Definition var) where
  pretty Definition' {name, definition, inferType} =
    pretty name
      <+> ":"
      <+> pretty inferType
      <+> "="
      <+> pretty definition


instance
  FreeVariables TypeVariableId var
  => FreeVariables TypeVariableId (Definition var)
  where
  freeVariables d =
    freeVariables d.definition <> freeVariables d.inferType


data Value var
  = VInt {intValue :: Text, inferType :: MonoType var}
  | VBool {boolValue :: Bool, inferType :: MonoType var}
  | Function
      { parameters :: NonEmpty (ExpressionVariableId, Type var)
      , body :: Expression var
      , inferType :: MonoType var
      }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (Value var)


getValueType :: Value var -> MonoType var
getValueType v = v.inferType


prettyParameterFunction
  :: Pretty var
  => (ExpressionVariableId, Type var)
  -> Doc ann
prettyParameterFunction (expr, t) =
  Pretty.parens
    ( pretty expr
        <+> ":"
        <+> pretty t
    )


prettyParametersFunction
  :: Pretty var
  => NonEmpty (ExpressionVariableId, Type var)
  -> Doc ann
prettyParametersFunction ps =
  (Pretty.vsep <<< toList)
    ( ( Pretty.group
          <<< prettyParameterFunction
      )
        <$> ps
    )


instance Pretty var => Pretty (Value var) where
  pretty VInt {intValue} = pretty @Text intValue
  pretty VBool {boolValue} = pretty boolValue
  pretty Function {parameters, body} =
    Pretty.vsep
      [ formatText "\\"
          <> Pretty.indent
            defaultIndentationSpaces
            ( Pretty.line
                <> prettyParametersFunction
                  parameters
            )
      , formatText "->"
          <> ( Pretty.group
                <<< Pretty.indent defaultIndentationSpaces
             )
            ( Pretty.line
                <> pretty body
            )
      ]


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
  = Variable {name :: ExpressionVariableId, inferType :: MonoType var}
  | EValue {value :: Value var, inferType :: MonoType var}
  | Application
      { applicationFunction :: Expression var
      , applicationArgument :: Expression var
      , inferType :: MonoType var
      }
  | If
      { condition :: Expression var
      , ifTrue :: Expression var
      , ifFalse :: Expression var
      , inferType :: MonoType var
      }
  | Let
      { definitions :: NonEmpty (Definition var)
      , expression :: Expression var
      , inferType :: MonoType var
      }
  | Annotation
      { expression :: Expression var
      , _type :: MonoType var
      , inferType :: MonoType var
      }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (Expression var)


instance From (Expression var) (Value var) where
  from v = EValue {value = v, inferType = getValueType v}


needsParentsInApplication :: Expression var -> Bool
needsParentsInApplication e =
  case e of
    EValue {value = VInt {}} -> False
    EValue {value = VBool {}} -> False
    EValue {value = Function {}} -> True
    Variable {} -> False
    Application {} -> True
    If {} -> True
    Let {} -> True
    Annotation {} -> True


instance Pretty var => Pretty (Expression var) where
  pretty EValue {value} = pretty value
  pretty Variable {name} = pretty name
  pretty Application {applicationFunction, applicationArgument, inferType} =
    (Pretty.group <<< Pretty.indent defaultIndentationSpaces)
      ( Pretty.line'
          <> prettyArg applicationFunction
          <> prettyArg applicationArgument
      )
    where
      prettyArg expr =
        if needsParentsInApplication expr
          then
            Pretty.parens
              (pretty expr)
          else pretty expr
  pretty If {condition, ifTrue, ifFalse} =
    (Pretty.group <<< Pretty.vsep)
      [ formatText "if"
          <> Pretty.indent
            defaultIndentationSpaces
            ( Pretty.line
                <> pretty condition
            )
      , formatText "then"
          <> Pretty.indent
            defaultIndentationSpaces
            ( Pretty.line
                <> pretty ifTrue
            )
      , formatText "else"
          <> Pretty.indent
            defaultIndentationSpaces
            ( Pretty.line
                <> pretty ifFalse
            )
      ]
  pretty Let {definitions, expression} =
    Pretty.parens
      ( (Pretty.group <<< Pretty.vsep)
          [ formatText "let"
              <> Pretty.indent
                defaultIndentationSpaces
                ( Pretty.line
                    <> (Pretty.vsep <<< toList)
                      ( ( (<> formatText ";")
                            <<< pretty
                        )
                          <$> definitions
                      )
                )
          , formatText
              "in"
              <> Pretty.indent
                defaultIndentationSpaces
                ( Pretty.line
                    <> pretty expression
                )
          ]
      )
  pretty Annotation {expression, inferType} =
    (Pretty.parens <<< Pretty.group)
      ( pretty expression
          <> Pretty.line
          <> Pretty.indent
            defaultIndentationSpaces
            ( formatText ":"
                <> Pretty.line
                <> pretty inferType
            )
      )


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
getType e = from e.inferType
