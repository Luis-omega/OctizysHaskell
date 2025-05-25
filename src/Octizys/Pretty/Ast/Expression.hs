module Octizys.Pretty.Ast.Expression where

import Control.Arrow ((<<<))
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Text (Text)
import Octizys.Ast.Expression
import Octizys.Ast.Type (Type)
import Octizys.Cst.Expression (ExpressionVariableId)
import qualified Octizys.Pretty.Ast.Type as Type
import Octizys.Pretty.FormatContext
  ( FormatContext
  , formatExpressionVar
  , formatText
  , nest
  , shouldShowTypes
  )
import Prettyprinter (Doc, Pretty (pretty), (<+>))
import qualified Prettyprinter as Pretty


formatDefinition
  :: FormatContext ann
  -> Definition
  -> Doc ann
formatDefinition ctx Definition' {name, definition, inferType} =
  formatExpressionVar ctx name
    <+> ":"
    <+> Type.format ctx inferType
    <+> "="
    <+> formatExpression ctx definition


formatParameterFunction
  :: FormatContext ann
  -> (ExpressionVariableId, Type)
  -> Doc ann
formatParameterFunction ctx (expr, t) =
  Pretty.parens (formatExpressionVar ctx expr <+> ":" <+> Type.format ctx t)


formatParametersFunction
  :: FormatContext ann
  -> NonEmpty (ExpressionVariableId, Type)
  -> Doc ann
formatParametersFunction ctx ps =
  (Pretty.vsep <<< toList)
    ( ( Pretty.group
          <<< formatParameterFunction ctx
      )
        <$> ps
    )


needsParentsInApplication :: Expression -> Bool
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


annotateType :: FormatContext ann -> Doc ann -> Type -> Doc ann
annotateType ctx doc t =
  if shouldShowTypes ctx
    then
      Pretty.parens
        ( Pretty.group
            ( doc
                <> nest
                  ctx
                  ( Pretty.line'
                      <> formatText ":"
                      <> Type.format ctx t
                  )
            )
        )
    else doc


formatValue :: FormatContext ann -> Value -> Doc ann
formatValue _ VInt {intValue} = pretty @Text intValue
formatValue _ VBool {boolValue} = pretty boolValue
formatValue ctx Function {parameters, body, inferType} =
  annotateType
    ctx
    ( Pretty.vsep
        [ formatText "\\"
            <> nest
              ctx
              ( Pretty.line
                  <> formatParametersFunction
                    ctx
                    parameters
              )
        , formatText "->"
            <> ( Pretty.group
                  <<< nest ctx
               )
              ( Pretty.line
                  <> formatExpression ctx body
              )
        ]
    )
    inferType


formatExpression
  :: FormatContext ann -> Expression -> Doc ann
formatExpression ctx EValue {value} = formatValue ctx value
formatExpression ctx Variable {name, inferType} =
  annotateType
    ctx
    ( formatExpressionVar ctx name
    )
    inferType
formatExpression ctx Application {applicationFunction, applicationArgument, inferType} =
  annotateType
    ctx
    ( (Pretty.group <<< nest ctx)
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
                ctx
                expr
            )
        else formatExpression ctx expr
formatExpression ctx If {condition, ifTrue, ifFalse, inferType} =
  annotateType
    ctx
    ( (Pretty.group <<< Pretty.vsep)
        [ formatText "if"
            <> nest
              ctx
              ( Pretty.line
                  <> formatExpression ctx condition
              )
        , formatText "then"
            <> nest
              ctx
              ( Pretty.line
                  <> formatExpression ctx ifTrue
              )
        , formatText "else"
            <> nest
              ctx
              ( Pretty.line
                  <> formatExpression ctx ifFalse
              )
        ]
    )
    inferType
formatExpression ctx Let {definitions, expression, inferType} =
  annotateType
    ctx
    ( Pretty.parens
        ( (Pretty.group <<< Pretty.vsep)
            [ formatText "let"
                <> nest
                  ctx
                  ( Pretty.line
                      <> (Pretty.vsep <<< toList)
                        ( ( (<> formatText ";")
                              <<< formatDefinition ctx
                          )
                            <$> definitions
                        )
                  )
            , formatText
                "in"
                <> nest
                  ctx
                  ( Pretty.line
                      <> formatExpression ctx expression
                  )
            ]
        )
    )
    inferType
formatExpression ctx Annotation {expression, inferType} =
  (Pretty.parens <<< Pretty.group)
    ( formatExpression ctx expression
        <> Pretty.line
        <> nest
          ctx
          ( formatText ":"
              <> Pretty.line
              <> Type.format ctx inferType
          )
    )
