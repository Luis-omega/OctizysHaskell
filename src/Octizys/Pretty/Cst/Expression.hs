module Octizys.Pretty.Cst.Expression where

import Control.Arrow ((<<<))
import Data.List.NonEmpty (NonEmpty (..), toList)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import Octizys.Cst.Expression
  ( Definition (Definition', definition, name, _type)
  , DefinitionTypeAnnotation
    ( DefinitionTypeAnnotation'
    , outputType
    , parameters
    , schemeStart
    )
  , Expression
    ( Annotation
    , Application
    , EBool
    , EFunction
    , EInt
    , If
    , Let
    , Parens
    , Variable
    , applicationFunction
    , applicationRemain
    , boolValue
    , condition
    , definitions
    , expression
    , functionValue
    , ifFalse
    , ifTrue
    , intValue
    , name
    , _type
    )
  , Function (Function', body, parameters)
  , FunctionParameter
    ( FunctionParameterAlone
    , FunctionParameterWithType
    , parameter
    )
  , Parameter (ParameterAlone, ParameterWithType, name, _type)
  , Parameters (Parameters', unParameters)
  , SchemeStart (SchemeStart', typeArguments)
  )
import qualified Octizys.Pretty.Cst.Type as Type
import Octizys.Pretty.FormatContext
  ( FormatContext
  , formatExpressionVar
  , formatTypeVar
  , nest
  )
import Prettyprinter (Doc, Pretty (pretty), (<+>))
import qualified Prettyprinter as Pretty


pText :: Text -> Doc ann
pText = pretty @Text


formatParameter
  :: FormatContext ann
  -> Parameter
  -> Doc ann
formatParameter
  ctx
  (ParameterAlone {name = (_, v)}) = formatExpressionVar ctx v
formatParameter
  ctx
  (ParameterWithType {name = (_, v), _type = t}) =
    formatExpressionVar ctx v
      <+> pText ":"
      <> nest
        ctx
        ( Pretty.line
            <> Type.format ctx t
        )


formatParameters
  :: FormatContext ann
  -> Parameters
  -> Doc ann
formatParameters ctx (Parameters' {unParameters}) =
  case unParameters of
    (start :| remain) ->
      (nest ctx <<< Pretty.vsep)
        ( Pretty.group
            ( formatParameter
                ctx
                (fst start)
            )
            : ( prettyArg
                  <$> remain
              )
        )
      where
        prettyArg (p, _) =
          pText ","
            <> nest
              ctx
              ( Pretty.line
                  <> formatParameter
                    ctx
                    p
              )


formatSchemeStart
  :: FormatContext ann
  -> SchemeStart
  -> Doc ann
formatSchemeStart ctx SchemeStart' {typeArguments} =
  pretty @Text "forall"
    <> Pretty.line
    <> nest
      ctx
      ( Pretty.fillSep
          (formatTypeVar ctx <$> NonEmpty.toList typeArguments)
      )
    <> Pretty.line
    <> pretty '.'


formatDefinitionTypeAnnotation
  :: FormatContext ann
  -> DefinitionTypeAnnotation
  -> Doc ann
formatDefinitionTypeAnnotation
  ctx
  DefinitionTypeAnnotation'
    { schemeStart
    , parameters
    , outputType
    } =
    let
      scheme = maybe mempty (formatSchemeStart ctx) schemeStart
      pars =
        case parameters of
          Just (ps, _) ->
            (nest ctx <<< Pretty.group)
              ( Pretty.line
                  <> formatParameters
                    ctx
                    ps
              )
          Nothing -> mempty
      outType =
        case parameters of
          Nothing ->
            Pretty.group
              ( Pretty.line <> Type.format ctx outputType
              )
          Just _ ->
            Pretty.group
              ( pretty ','
                  <> Pretty.line
                  <> Type.format ctx outputType
              )
     in
      pretty ':'
        <> scheme
        <> pars
        <> nest ctx outType


formatDefinition
  :: FormatContext ann
  -> Definition
  -> Doc ann
formatDefinition
  ctx
  (Definition' {name = (_, v), _type, definition}) =
    let n = formatExpressionVar ctx v
        def =
          ( Pretty.line
              <> formatExpression ctx definition
          )
     in n
          <> maybe mempty (formatDefinitionTypeAnnotation ctx) _type
          <> (Pretty.group <<< nest ctx)
            ( Pretty.line
                <> pText "="
                <> Pretty.group def
            )


formatParameterFunction
  :: FormatContext ann
  -> FunctionParameter
  -> Doc ann
formatParameterFunction
  ctx
  (FunctionParameterWithType {parameter = p}) =
    pText "("
      <> Pretty.nest
        2
        ( Pretty.line
            <> formatParameter ctx p
            <> Pretty.line
        )
      <> pText ")"
formatParameterFunction
  ctx
  (FunctionParameterAlone {parameter = p}) =
    formatParameter ctx p


prettyParametersFunction
  :: FormatContext ann
  -> NonEmpty FunctionParameter
  -> Doc ann
prettyParametersFunction ctx ps =
  (Pretty.vsep <<< toList)
    ( ( Pretty.group
          <<< formatParameterFunction ctx
      )
        <$> ps
    )


formatFunction
  :: FormatContext ann
  -> Function
  -> Doc ann
formatFunction ctx (Function' {parameters, body}) =
  Pretty.vsep
    [ pText "\\"
        <> nest
          ctx
          ( Pretty.line
              <> prettyParametersFunction
                ctx
                parameters
          )
    , pText "->"
        <> ( Pretty.group
              <<< nest ctx
           )
          ( Pretty.line
              <> formatExpression ctx body
          )
    ]


needsParentsInApplication :: Expression -> Bool
needsParentsInApplication e =
  case e of
    EInt {} -> False
    EBool {} -> False
    Variable {} -> False
    Parens {} -> False
    EFunction {} -> True
    Application {} -> True
    If {} -> True
    Let {} -> True
    Annotation {} -> True


formatExpression
  :: FormatContext ann
  -> Expression
  -> Doc ann
formatExpression ctx e =
  case e of
    EInt {intValue} -> pretty intValue
    EBool {boolValue} ->
      pText $ if boolValue then "True" else "False"
    Variable {name} -> formatExpressionVar ctx name
    Parens {expression} ->
      Pretty.parens $
        formatExpression
          ctx
          expression
    EFunction {functionValue} ->
      formatFunction
        ctx
        functionValue
    Application
      { applicationFunction =
        _function
      , applicationRemain = _arguments
      } ->
        (Pretty.group <<< nest ctx)
          ( Pretty.line'
              <> prettyArg _function
              <> nest
                ctx
                ( Pretty.line
                    <> (Pretty.vsep <<< toList)
                      (prettyArg <$> _arguments)
                )
          )
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
    If {condition = _condition, ifTrue = __then, ifFalse = __else} ->
      (Pretty.group <<< Pretty.vsep)
        [ pText "if"
            <> nest
              ctx
              ( Pretty.line
                  <> formatExpression ctx _condition
              )
        , pText "then"
            <> nest
              ctx
              ( Pretty.line
                  <> formatExpression ctx __then
              )
        , pText "else"
            <> nest
              ctx
              ( Pretty.line
                  <> formatExpression ctx __else
              )
        ]
    Let {definitions, expression = _in} ->
      (Pretty.group <<< Pretty.vsep)
        [ pText "let"
            <> Pretty.nest
              2
              ( Pretty.line
                  <> (Pretty.vsep <<< toList)
                    ( ( (<> pText ";")
                          <<< formatDefinition ctx
                          <<< fst
                      )
                        <$> definitions
                    )
              )
        , pText
            "in"
            <> Pretty.nest
              2
              ( Pretty.line
                  <> formatExpression ctx _in
              )
        ]
    Annotation {expression = _expression, _type} ->
      (Pretty.parens <<< Pretty.group)
        ( formatExpression ctx _expression
            <> Pretty.line
            <> nest
              ctx
              ( pText ":"
                  <> Pretty.line
                  <> Type.format ctx _type
              )
        )
