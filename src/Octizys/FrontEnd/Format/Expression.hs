module Octizys.FrontEnd.Format.Expression where

import Control.Arrow ((<<<))
import Data.List.NonEmpty (toList)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import Octizys.Common.Format.Config
  ( nest
  )
import qualified Octizys.Common.Format.Config as Format
import Octizys.FrontEnd.Cst.Expression
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
import qualified Octizys.FrontEnd.Format.Type as Type
import Prettyprinter (Doc, Pretty (pretty), (<+>))
import qualified Prettyprinter as Pretty


pText :: Text -> Doc ann
pText = pretty @Text


formatParameter
  :: Pretty tvar
  => Pretty evar
  => Format.Configuration
  -> Parameter evar tvar
  -> Doc ann
formatParameter
  _
  (ParameterAlone {name = (_, v)}) = pretty v
formatParameter
  configuration
  (ParameterWithType {name = (_, v), _type = t}) =
    pretty v
      <+> pText ":"
      <> nest
        configuration
        ( Pretty.line
            <> Type.format configuration t
        )


formatParameters
  :: Pretty tvar
  => Pretty evar
  => Format.Configuration
  -> Parameters evar tvar
  -> Doc ann
formatParameters
  configuration
  (Parameters' {initParameter, otherParameters}) =
    Pretty.vsep
      ( Pretty.group
          ( formatParameter
              configuration
              initParameter
          )
          : ( prettyArg
                <$> otherParameters
            )
      )
      <> Pretty.line
    where
      prettyArg (_, p) =
        pText ","
          <> nest
            configuration
            ( Pretty.line
                <> formatParameter
                  configuration
                  p
            )


formatSchemeStart
  :: Pretty tvar
  => Format.Configuration
  -> SchemeStart tvar
  -> Doc ann
formatSchemeStart configuration SchemeStart' {typeArguments} =
  pretty @Text "forall"
    <> Pretty.line
    <> nest
      configuration
      ( Pretty.fillSep
          ((pretty <<< snd) <$> NonEmpty.toList typeArguments)
      )
    <> Pretty.line
    <> pretty '.'


formatDefinitionTypeAnnotation
  :: Pretty tvar
  => Pretty evar
  => Format.Configuration
  -> DefinitionTypeAnnotation evar tvar
  -> Doc ann
formatDefinitionTypeAnnotation
  configuration
  DefinitionTypeAnnotation'
    { schemeStart
    , parameters
    , outputType
    } =
    let
      scheme = maybe mempty (formatSchemeStart configuration) schemeStart
      pars =
        case parameters of
          Just ps ->
            (nest configuration <<< Pretty.group)
              ( Pretty.line
                  <> formatParameters
                    configuration
                    ps
              )
          Nothing -> mempty
      outType =
        case parameters of
          Nothing ->
            (nest configuration <<< Pretty.group)
              ( Pretty.line <> Type.format configuration outputType
              )
          Just _ ->
            Pretty.group
              ( pretty @Text "|-"
                  <> Pretty.line
                  <> Type.format configuration outputType
              )
     in
      pretty ':'
        <> scheme
        <> pars
        <> nest configuration outType


formatDefinition
  :: Pretty evar
  => Pretty tvar
  => Format.Configuration
  -> Definition evar tvar
  -> Doc ann
formatDefinition
  configuration
  (Definition' {name = (_, v), _type, definition}) =
    let n = pretty v
        def =
          ( Pretty.line
              <> formatExpression configuration definition
          )
     in n
          <> maybe
            mempty
            (formatDefinitionTypeAnnotation configuration)
            _type
          <> (Pretty.group <<< nest configuration)
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
  :: Pretty evar
  => Pretty tvar
  => Format.Configuration
  -> Expression evar tvar
  -> Doc ann
formatExpression configuration e =
  case e of
    EInt {intValue} -> pretty intValue
    EBool {boolValue} ->
      pText $ if boolValue then "True" else "False"
    Variable {name} -> pretty name
    Parens {expression} ->
      Pretty.parens $
        formatExpression
          configuration
          expression
    EFunction {parameters, body} ->
      Pretty.vsep
        [ pText "\\"
            <> nest
              configuration
              ( Pretty.line
                  <> formatParameters
                    configuration
                    parameters
              )
        , pretty @Text "|-"
            <> ( Pretty.group
                  <<< nest configuration
               )
              ( Pretty.line
                  <> formatExpression
                    configuration
                    body
              )
        ]
    Application
      { applicationFunction =
        _function
      , applicationRemain = _arguments
      } ->
        (Pretty.group <<< nest configuration)
          ( Pretty.line'
              <> prettyArg _function
              <> nest
                configuration
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
                      configuration
                      expr
                  )
              else
                formatExpression
                  configuration
                  expr
    If {condition = _condition, ifTrue = __then, ifFalse = __else} ->
      (Pretty.group <<< Pretty.vsep)
        [ pText "if"
            <> nest
              configuration
              ( Pretty.line
                  <> formatExpression
                    configuration
                    _condition
              )
        , pText "then"
            <> nest
              configuration
              ( Pretty.line
                  <> formatExpression
                    configuration
                    __then
              )
        , pText "else"
            <> nest
              configuration
              ( Pretty.line
                  <> formatExpression
                    configuration
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
                          <<< formatDefinition configuration
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
                    configuration
                    _in
              )
        ]
    Annotation {expression = _expression, _type} ->
      (Pretty.parens <<< Pretty.group)
        ( formatExpression
            configuration
            _expression
            <> Pretty.line
            <> nest
              configuration
              ( pText ":"
                  <> Pretty.line
                  <> Type.format configuration _type
              )
        )
