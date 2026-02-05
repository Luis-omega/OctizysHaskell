module Octizys.Ast.Format.Expression where

import Control.Arrow ((<<<))
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Text (Text)
import Octizys.Ast.Expression
import qualified Octizys.Ast.Format.Type as Type
import Octizys.Ast.Type (MonoType)
import Octizys.Classes.From (from)
import Octizys.Common.Format.Config
  ( formatText
  , nest
  , shouldShowTypes
  )
import qualified Octizys.Common.Format.Config as Format
import Octizys.Common.Id (ExpressionVariableId)
import Prettyprinter (Doc, Pretty (pretty), (<+>))
import qualified Prettyprinter as Pretty


formatDefinition
  :: (Format.Configuration -> var -> Doc ann)
  -> Format.Configuration
  -> Definition var
  -> Doc ann
formatDefinition fmtVar configuration Definition' {name, definition, inferType} =
  pretty name
    <+> ":"
    <+> Type.format fmtVar configuration inferType
    <+> "="
    <+> formatExpression fmtVar configuration definition


formatParameterFunction
  :: (Format.Configuration -> var -> Doc ann)
  -> Format.Configuration
  -> (ExpressionVariableId, MonoType var)
  -> Doc ann
formatParameterFunction fmtVar configuration (expr, t) =
  Pretty.parens
    ( pretty expr
        <+> ":"
        <+> Type.format fmtVar configuration (from t)
    )


formatParametersFunction
  :: (Format.Configuration -> var -> Doc ann)
  -> Format.Configuration
  -> NonEmpty (ExpressionVariableId, MonoType var)
  -> Doc ann
formatParametersFunction fmtVar configuration ps =
  (Pretty.vsep <<< toList)
    ( ( Pretty.group
          <<< formatParameterFunction fmtVar configuration
      )
        <$> ps
    )


annotateType
  :: (Format.Configuration -> var -> Doc ann)
  -> Format.Configuration
  -> Doc ann
  -> MonoType var
  -> Doc ann
annotateType fmtVar configuration doc t =
  if shouldShowTypes configuration
    then
      Pretty.parens
        ( Pretty.group
            ( doc
                <> nest
                  configuration
                  ( Pretty.line'
                      <> formatText ":"
                      <> Type.formatMono fmtVar configuration t
                  )
            )
        )
    else doc


formatValue
  :: (Format.Configuration -> var -> Doc ann)
  -> Format.Configuration
  -> Value var
  -> Doc ann
formatValue _ _ VInt {intValue} = pretty @Text intValue
formatValue _ _ VBool {boolValue} = pretty boolValue
formatValue fmtVar configuration Function {parameters, body, inferType} =
  annotateType
    fmtVar
    configuration
    ( Pretty.vsep
        [ formatText "\\"
            <> nest
              configuration
              ( Pretty.line
                  <> formatParametersFunction
                    fmtVar
                    configuration
                    parameters
              )
        , formatText "->"
            <> ( Pretty.group
                  <<< nest configuration
               )
              ( Pretty.line
                  <> formatExpression fmtVar configuration body
              )
        ]
    )
    inferType


formatExpression
  :: (Format.Configuration -> var -> Doc ann)
  -> Format.Configuration
  -> Expression var
  -> Doc ann
formatExpression fmtVar configuration EValue {value} = formatValue fmtVar configuration value
formatExpression fmtVar configuration Variable {name, inferType} =
  annotateType
    fmtVar
    configuration
    ( pretty name
    )
    inferType
formatExpression fmtVar configuration Application {applicationFunction, applicationArgument, inferType} =
  annotateType
    fmtVar
    configuration
    ( (Pretty.group <<< nest configuration)
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
                fmtVar
                configuration
                expr
            )
        else formatExpression fmtVar configuration expr
formatExpression fmtVar configuration If {condition, ifTrue, ifFalse, inferType} =
  annotateType
    fmtVar
    configuration
    ( (Pretty.group <<< Pretty.vsep)
        [ formatText "if"
            <> nest
              configuration
              ( Pretty.line
                  <> formatExpression fmtVar configuration condition
              )
        , formatText "then"
            <> nest
              configuration
              ( Pretty.line
                  <> formatExpression fmtVar configuration ifTrue
              )
        , formatText "else"
            <> nest
              configuration
              ( Pretty.line
                  <> formatExpression fmtVar configuration ifFalse
              )
        ]
    )
    inferType
formatExpression fmtVar configuration Let {definitions, expression, inferType} =
  annotateType
    fmtVar
    configuration
    ( Pretty.parens
        ( (Pretty.group <<< Pretty.vsep)
            [ formatText "let"
                <> nest
                  configuration
                  ( Pretty.line
                      <> (Pretty.vsep <<< toList)
                        ( ( (<> formatText ";")
                              <<< formatDefinition fmtVar configuration
                          )
                            <$> definitions
                        )
                  )
            , formatText
                "in"
                <> nest
                  configuration
                  ( Pretty.line
                      <> formatExpression fmtVar configuration expression
                  )
            ]
        )
    )
    inferType
formatExpression fmtVar configuration Annotation {expression, inferType} =
  (Pretty.parens <<< Pretty.group)
    ( formatExpression fmtVar configuration expression
        <> Pretty.line
        <> nest
          configuration
          ( formatText ":"
              <> Pretty.line
              <> Type.formatMono fmtVar configuration inferType
          )
    )
