module Octizys.Pretty.Ast.Expression where

import Control.Arrow ((<<<))
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Text (Text)
import Octizys.Ast.Expression
import Octizys.Ast.Type (Type)
import Octizys.Common.Id (ExpressionVariableId)
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
  :: (FormatContext ann -> var -> Doc ann)
  -> FormatContext ann
  -> Definition var
  -> Doc ann
formatDefinition fmtVar ctx Definition' {name, definition, inferType} =
  formatExpressionVar ctx name
    <+> ":"
    <+> Type.format fmtVar ctx inferType
    <+> "="
    <+> formatExpression fmtVar ctx definition


formatParameterFunction
  :: (FormatContext ann -> var -> Doc ann)
  -> FormatContext ann
  -> (ExpressionVariableId, Type var)
  -> Doc ann
formatParameterFunction fmtVar ctx (expr, t) =
  Pretty.parens
    (formatExpressionVar ctx expr <+> ":" <+> Type.format fmtVar ctx t)


formatParametersFunction
  :: (FormatContext ann -> var -> Doc ann)
  -> FormatContext ann
  -> NonEmpty (ExpressionVariableId, Type var)
  -> Doc ann
formatParametersFunction fmtVar ctx ps =
  (Pretty.vsep <<< toList)
    ( ( Pretty.group
          <<< formatParameterFunction fmtVar ctx
      )
        <$> ps
    )


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


annotateType
  :: (FormatContext ann -> var -> Doc ann)
  -> FormatContext ann
  -> Doc ann
  -> Type var
  -> Doc ann
annotateType fmtVar ctx doc t =
  if shouldShowTypes ctx
    then
      Pretty.parens
        ( Pretty.group
            ( doc
                <> nest
                  ctx
                  ( Pretty.line'
                      <> formatText ":"
                      <> Type.format fmtVar ctx t
                  )
            )
        )
    else doc


formatValue
  :: (FormatContext ann -> var -> Doc ann)
  -> FormatContext ann
  -> Value var
  -> Doc ann
formatValue _ _ VInt {intValue} = pretty @Text intValue
formatValue _ _ VBool {boolValue} = pretty boolValue
formatValue fmtVar ctx Function {parameters, body, inferType} =
  annotateType
    fmtVar
    ctx
    ( Pretty.vsep
        [ formatText "\\"
            <> nest
              ctx
              ( Pretty.line
                  <> formatParametersFunction
                    fmtVar
                    ctx
                    parameters
              )
        , formatText "->"
            <> ( Pretty.group
                  <<< nest ctx
               )
              ( Pretty.line
                  <> formatExpression fmtVar ctx body
              )
        ]
    )
    inferType


formatExpression
  :: (FormatContext ann -> var -> Doc ann)
  -> FormatContext ann
  -> Expression var
  -> Doc ann
formatExpression fmtVar ctx EValue {value} = formatValue fmtVar ctx value
formatExpression fmtVar ctx Variable {name, inferType} =
  annotateType
    fmtVar
    ctx
    ( formatExpressionVar ctx name
    )
    inferType
formatExpression fmtVar ctx Application {applicationFunction, applicationArgument, inferType} =
  annotateType
    fmtVar
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
                fmtVar
                ctx
                expr
            )
        else formatExpression fmtVar ctx expr
formatExpression fmtVar ctx If {condition, ifTrue, ifFalse, inferType} =
  annotateType
    fmtVar
    ctx
    ( (Pretty.group <<< Pretty.vsep)
        [ formatText "if"
            <> nest
              ctx
              ( Pretty.line
                  <> formatExpression fmtVar ctx condition
              )
        , formatText "then"
            <> nest
              ctx
              ( Pretty.line
                  <> formatExpression fmtVar ctx ifTrue
              )
        , formatText "else"
            <> nest
              ctx
              ( Pretty.line
                  <> formatExpression fmtVar ctx ifFalse
              )
        ]
    )
    inferType
formatExpression fmtVar ctx Let {definitions, expression, inferType} =
  annotateType
    fmtVar
    ctx
    ( Pretty.parens
        ( (Pretty.group <<< Pretty.vsep)
            [ formatText "let"
                <> nest
                  ctx
                  ( Pretty.line
                      <> (Pretty.vsep <<< toList)
                        ( ( (<> formatText ";")
                              <<< formatDefinition fmtVar ctx
                          )
                            <$> definitions
                        )
                  )
            , formatText
                "in"
                <> nest
                  ctx
                  ( Pretty.line
                      <> formatExpression fmtVar ctx expression
                  )
            ]
        )
    )
    inferType
formatExpression fmtVar ctx Annotation {expression, inferType} =
  (Pretty.parens <<< Pretty.group)
    ( formatExpression fmtVar ctx expression
        <> Pretty.line
        <> nest
          ctx
          ( formatText ":"
              <> Pretty.line
              <> Type.format fmtVar ctx inferType
          )
    )
