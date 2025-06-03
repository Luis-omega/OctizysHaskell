module Octizys.Pretty.Cst.Expression where

import Control.Arrow ((<<<))
import Data.List.NonEmpty (toList)
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
    , body
    , boolValue
    , condition
    , definitions
    , expression
    , ifFalse
    , ifTrue
    , intValue
    , name
    , parameters
    , _type
    )
  , Parameter (ParameterAlone, ParameterWithType, name, _type)
  , Parameters (Parameters', initParameter, otherParameters)
  , SchemeStart (SchemeStart', typeArguments)
  )
import qualified Octizys.Pretty.Cst.Type as Type
import Octizys.Pretty.FormatContext
  ( FormatContext
  , nest
  )
import Prettyprinter (Doc, Pretty (pretty), (<+>))
import qualified Prettyprinter as Pretty


pText :: Text -> Doc ann
pText = pretty @Text


formatParameter
  :: ( FormatContext ann
       -> evar
       -> Doc ann
     )
  -> ( FormatContext ann
       -> tvar
       -> Doc ann
     )
  -> FormatContext ann
  -> Parameter evar tvar
  -> Doc ann
formatParameter
  formatEvar
  _
  ctx
  (ParameterAlone {name = (_, v)}) = formatEvar ctx v
formatParameter
  formatEvar
  formatTvar
  ctx
  (ParameterWithType {name = (_, v), _type = t}) =
    formatEvar ctx v
      <+> pText ":"
      <> nest
        ctx
        ( Pretty.line
            <> Type.format formatTvar ctx t
        )


formatParameters
  :: ( FormatContext ann
       -> evar
       -> Doc ann
     )
  -> ( FormatContext ann
       -> tvar
       -> Doc ann
     )
  -> FormatContext ann
  -> Parameters evar tvar
  -> Doc ann
formatParameters
  formatEvar
  formatTvar
  ctx
  (Parameters' {initParameter, otherParameters}) =
    Pretty.vsep
      ( Pretty.group
          ( formatParameter
              formatEvar
              formatTvar
              ctx
              initParameter
          )
          : ( prettyArg
                <$> otherParameters
            )
      )
      <> Pretty.line
      <> pText "|-"
    where
      prettyArg (_, p) =
        pText ","
          <> nest
            ctx
            ( Pretty.line
                <> formatParameter
                  formatEvar
                  formatTvar
                  ctx
                  p
            )


formatSchemeStart
  :: ( FormatContext ann
       -> tvar
       -> Doc ann
     )
  -> FormatContext ann
  -> SchemeStart tvar
  -> Doc ann
formatSchemeStart fmtTvar ctx SchemeStart' {typeArguments} =
  pretty @Text "forall"
    <> Pretty.line
    <> nest
      ctx
      ( Pretty.fillSep
          ((fmtTvar ctx <<< snd) <$> NonEmpty.toList typeArguments)
      )
    <> Pretty.line
    <> pretty '.'


formatDefinitionTypeAnnotation
  :: ( FormatContext ann
       -> evar
       -> Doc ann
     )
  -> ( FormatContext ann
       -> tvar
       -> Doc ann
     )
  -> FormatContext ann
  -> DefinitionTypeAnnotation evar tvar
  -> Doc ann
formatDefinitionTypeAnnotation
  fmtEvar
  fmtTvar
  ctx
  DefinitionTypeAnnotation'
    { schemeStart
    , parameters
    , outputType
    } =
    let
      scheme = maybe mempty (formatSchemeStart fmtTvar ctx) schemeStart
      pars =
        case parameters of
          Just ps ->
            (nest ctx <<< Pretty.group)
              ( Pretty.line
                  <> formatParameters
                    fmtEvar
                    fmtTvar
                    ctx
                    ps
              )
          Nothing -> mempty
      outType =
        case parameters of
          Nothing ->
            (nest ctx <<< Pretty.group)
              ( Pretty.line <> Type.format fmtTvar ctx outputType
              )
          Just _ ->
            Pretty.group
              ( pretty ','
                  <> Pretty.line
                  <> Type.format fmtTvar ctx outputType
              )
     in
      pretty ':'
        <> scheme
        <> pars
        <> nest ctx outType


formatDefinition
  :: ( FormatContext ann
       -> evar
       -> Doc ann
     )
  -> ( FormatContext ann
       -> tvar
       -> Doc ann
     )
  -> FormatContext ann
  -> Definition evar tvar
  -> Doc ann
formatDefinition
  fmtEvar
  fmtTvar
  ctx
  (Definition' {name = (_, v), _type, definition}) =
    let n = fmtEvar ctx v
        def =
          ( Pretty.line
              <> formatExpression fmtEvar fmtTvar ctx definition
          )
     in n
          <> maybe mempty (formatDefinitionTypeAnnotation fmtEvar fmtTvar ctx) _type
          <> (Pretty.group <<< nest ctx)
            ( Pretty.line
                <> pText "="
                <> Pretty.group def
            )


needsParentsInApplication :: Expression evar tvar -> Bool
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
  :: ( FormatContext ann
       -> evar
       -> Doc ann
     )
  -> ( FormatContext ann
       -> tvar
       -> Doc ann
     )
  -> FormatContext ann
  -> Expression evar tvar
  -> Doc ann
formatExpression fmtEvar fmtTvar ctx e =
  case e of
    EInt {intValue} -> pretty intValue
    EBool {boolValue} ->
      pText $ if boolValue then "True" else "False"
    Variable {name} -> fmtEvar ctx name
    Parens {expression} ->
      Pretty.parens $
        formatExpression
          fmtEvar
          fmtTvar
          ctx
          expression
    EFunction {parameters, body} ->
      Pretty.vsep
        [ pText "\\"
            <> nest
              ctx
              ( Pretty.line
                  <> formatParameters
                    fmtEvar
                    fmtTvar
                    ctx
                    parameters
              )
        , ( Pretty.group
              <<< nest ctx
          )
            ( Pretty.line
                <> formatExpression
                  fmtEvar
                  fmtTvar
                  ctx
                  body
            )
        ]
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
                      fmtEvar
                      fmtTvar
                      ctx
                      expr
                  )
              else
                formatExpression
                  fmtEvar
                  fmtTvar
                  ctx
                  expr
    If {condition = _condition, ifTrue = __then, ifFalse = __else} ->
      (Pretty.group <<< Pretty.vsep)
        [ pText "if"
            <> nest
              ctx
              ( Pretty.line
                  <> formatExpression
                    fmtEvar
                    fmtTvar
                    ctx
                    _condition
              )
        , pText "then"
            <> nest
              ctx
              ( Pretty.line
                  <> formatExpression
                    fmtEvar
                    fmtTvar
                    ctx
                    __then
              )
        , pText "else"
            <> nest
              ctx
              ( Pretty.line
                  <> formatExpression
                    fmtEvar
                    fmtTvar
                    ctx
                    __else
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
                          <<< formatDefinition fmtEvar fmtTvar ctx
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
                  <> formatExpression
                    fmtEvar
                    fmtTvar
                    ctx
                    _in
              )
        ]
    Annotation {expression = _expression, _type} ->
      (Pretty.parens <<< Pretty.group)
        ( formatExpression
            fmtEvar
            fmtTvar
            ctx
            _expression
            <> Pretty.line
            <> nest
              ctx
              ( pText ":"
                  <> Pretty.line
                  <> Type.format fmtTvar ctx _type
              )
        )
