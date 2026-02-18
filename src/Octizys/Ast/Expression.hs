{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

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
import Octizys.Format.Class (Formattable, format)
import qualified Octizys.Format.Config as Format
import qualified Octizys.Format.Utils as Format
import Prettyprinter (Doc, Pretty (pretty), (<+>))
import qualified Prettyprinter as Pretty


data Definition var = Definition'
  { name :: ExpressionVariableId
  , definition :: Expression var
  , inferType :: Type var
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (Definition var)


instance Formattable var => Formattable (Definition var) where
  format = formatDefinition


instance
  ( FreeVariables TypeVariableId (Type var)
  , FreeVariables TypeVariableId (MonoType var)
  , FreeVariables TypeVariableId var
  )
  => FreeVariables TypeVariableId (Definition var)
  where
  freeVariables d =
    freeVariables d.definition <> freeVariables d.inferType


data Value var
  = VInt {intValue :: Text, inferType :: MonoType var}
  | VBool {boolValue :: Bool, inferType :: MonoType var}
  | Function
      { parameters :: NonEmpty (ExpressionVariableId, MonoType var)
      , body :: Expression var
      , inferType :: MonoType var
      }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (Value var)


instance Formattable var => Formattable (Value var) where
  format = formatValue


getValueType :: Value var -> MonoType var
getValueType v = v.inferType


formatParameterFunction
  :: Formattable var
  => Format.Configuration
  -> (ExpressionVariableId, MonoType var)
  -> Doc ann
formatParameterFunction c (expr, t) =
  Pretty.parens
    ( pretty expr
        <+> ":"
        <+> format c t
    )


formatParametersFunction
  :: Formattable var
  => Format.Configuration
  -> NonEmpty (ExpressionVariableId, MonoType var)
  -> Doc ann
formatParametersFunction c ps =
  (Pretty.vsep <<< toList)
    ( ( Pretty.group
          <<< formatParameterFunction c
      )
        <$> ps
    )


instance
  ( FreeVariables TypeVariableId (MonoType var)
  , FreeVariables TypeVariableId (Type var)
  , FreeVariables TypeVariableId var
  )
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


instance Formattable var => Formattable (Expression var) where
  format = formatExpression


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
  ( FreeVariables TypeVariableId (MonoType var)
  , FreeVariables TypeVariableId (Type var)
  , FreeVariables TypeVariableId var
  )
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


getMonoType :: HasCallStack => Expression var -> MonoType var
getMonoType e = e.inferType


getType :: HasCallStack => Expression var -> MonoType var
getType e = e.inferType


-- * Format


formatDefinition
  :: Formattable var
  => Format.Configuration
  -> Definition var
  -> Doc ann
formatDefinition configuration Definition' {name, definition, inferType} =
  pretty name
    <+> ":"
    <+> format configuration inferType
    <+> "="
    <+> formatExpression configuration definition


annotateType
  :: Formattable var
  => Format.Configuration
  -> Doc ann
  -> MonoType var
  -> Doc ann
annotateType configuration doc t =
  if Format.shouldAstShowTypes configuration
    then
      Pretty.parens
        ( Pretty.group
            ( doc
                <> Format.nest
                  configuration
                  ( Pretty.line'
                      <> Format.text ":"
                      <> format configuration t
                  )
            )
        )
    else doc


formatValue
  :: Formattable var
  => Format.Configuration
  -> Value var
  -> Doc ann
formatValue _ VInt {intValue} = pretty @Text intValue
formatValue _ VBool {boolValue} = pretty boolValue
formatValue configuration Function {parameters, body, inferType} =
  annotateType
    configuration
    ( Pretty.vsep
        [ Format.functionStart
            <> Format.nest
              configuration
              ( Pretty.line
                  <> formatParametersFunction
                    configuration
                    parameters
              )
        , Format.functionBodySeparator
            <> ( Pretty.group
                  <<< Format.nest configuration
               )
              ( Pretty.line
                  <> formatExpression configuration body
              )
        ]
    )
    inferType


formatExpression
  :: Formattable var
  => Format.Configuration
  -> Expression var
  -> Doc ann
formatExpression configuration EValue {value} = formatValue configuration value
formatExpression configuration Variable {name, inferType} =
  annotateType
    configuration
    ( pretty name
    )
    inferType
formatExpression configuration Application {applicationFunction, applicationArgument, inferType} =
  annotateType
    configuration
    ( (Pretty.group <<< Format.nest configuration)
        ( Pretty.line'
            <> prettyArg applicationFunction
            <> prettyArg applicationArgument
        )
    )
    inferType
  where
    prettyArg expr =
      if needsParentsInApplication expr
        then
          Pretty.parens
            ( formatExpression
                configuration
                expr
            )
        else formatExpression configuration expr
formatExpression configuration If {condition, ifTrue, ifFalse, inferType} =
  annotateType
    configuration
    ( (Pretty.group <<< Pretty.vsep)
        [ Format.text "if"
            <> Format.nest
              configuration
              ( Pretty.line
                  <> formatExpression configuration condition
              )
        , Format.text "then"
            <> Format.nest
              configuration
              ( Pretty.line
                  <> formatExpression configuration ifTrue
              )
        , Format.text "else"
            <> Format.nest
              configuration
              ( Pretty.line
                  <> formatExpression configuration ifFalse
              )
        ]
    )
    inferType
formatExpression configuration Let {definitions, expression, inferType} =
  annotateType
    configuration
    ( Pretty.parens
        ( (Pretty.group <<< Pretty.vsep)
            [ Format.text "let"
                <> Format.nest
                  configuration
                  ( Pretty.line
                      <> (Pretty.vsep <<< toList)
                        ( ( (<> Format.text ";")
                              <<< formatDefinition configuration
                          )
                            <$> definitions
                        )
                  )
            , Format.text
                "in"
                <> Format.nest
                  configuration
                  ( Pretty.line
                      <> formatExpression configuration expression
                  )
            ]
        )
    )
    inferType
formatExpression configuration Annotation {expression, inferType} =
  (Pretty.parens <<< Pretty.group)
    ( formatExpression configuration expression
        <> Pretty.line
        <> Format.nest
          configuration
          ( Format.text ":"
              <> Pretty.line
              <> format configuration inferType
          )
    )
