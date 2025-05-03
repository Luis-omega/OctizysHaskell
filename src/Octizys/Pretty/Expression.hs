module Octizys.Pretty.Expression where

import Control.Arrow ((<<<))
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Text (Text)
import Octizys.Cst.Expression
  ( Definition (Definition', definition, name, outputType, parameters)
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
  , ExpressionVariableId
  , Function (Function', body, parameters)
  , FunctionParameter
    ( FunctionParameterAlone
    , FunctionParameterWithType
    , parameter
    )
  , Parameter (ParameterAlone, ParameterWithType, name, _type)
  , Parameters (Parameters', unParameters)
  )
import Octizys.Cst.Type (TypeVariableId)
import Octizys.Pretty.Type (prettyType)
import Prettyprinter (Doc, Pretty (pretty), (<+>))
import qualified Prettyprinter as Pretty


pText :: Text -> Doc ann
pText = pretty @Text


prettyParameter
  :: (ExpressionVariableId -> Doc ann)
  -> (TypeVariableId -> Doc ann)
  -> Parameter
  -> Doc ann
prettyParameter
  prettyVar
  _
  (ParameterAlone {name = (_, v)}) = prettyVar v
prettyParameter
  prettyVar
  prettyTypeVar
  (ParameterWithType {name = (_, v), _type = t}) =
    prettyVar v
      <+> pText ":"
      <> Pretty.nest
        2
        ( Pretty.line
            <> prettyType prettyTypeVar t
        )


prettyParameters
  :: (ExpressionVariableId -> Doc ann)
  -> (TypeVariableId -> Doc ann)
  -> Parameters
  -> Doc ann
prettyParameters prettyVar prettyTypeVar (Parameters' {unParameters}) =
  case unParameters of
    (start :| remain) ->
      (Pretty.nest 2 <<< Pretty.vsep)
        ( Pretty.group
            ( prettyParameter
                prettyVar
                prettyTypeVar
                (fst start)
            )
            : ( prettyArg
                  <$> remain
              )
        )
      where
        prettyArg (p, _) =
          pText ","
            <> Pretty.nest
              2
              ( Pretty.line
                  <> prettyParameter
                    prettyVar
                    prettyTypeVar
                    p
              )


prettyDefinition
  :: (ExpressionVariableId -> Doc ann)
  -> (TypeVariableId -> Doc ann)
  -> Definition
  -> Doc ann
prettyDefinition
  prettyVar
  prettyTypeVar
  (Definition' {name = (_, v), parameters, definition, outputType}) =
    let n = prettyVar v
        pars =
          case parameters of
            Just ps ->
              pText ""
                <+> pretty ':'
                <> (Pretty.nest 2 <<< Pretty.group)
                  ( Pretty.line
                      <> prettyParameters
                        prettyVar
                        prettyTypeVar
                        ps
                  )
            Nothing -> mempty
        outType =
          case outputType of
            Just t ->
              case parameters of
                Nothing ->
                  Pretty.group
                    ( pretty ':'
                        <> (Pretty.line <> prettyType prettyTypeVar t)
                    )
                Just _ ->
                  Pretty.group
                    ( pretty ','
                        <> Pretty.line
                        <> prettyType prettyTypeVar t
                    )
            Nothing -> mempty
        def =
          ( Pretty.line
              <> prettyExpression prettyVar prettyTypeVar definition
          )
     in n
          <> pars
          <> outType
          <> (Pretty.group <<< Pretty.nest 2)
            ( Pretty.line
                <> pText "="
                <> Pretty.group def
            )


prettyParameterFunction
  :: (ExpressionVariableId -> Doc ann)
  -> (TypeVariableId -> Doc ann)
  -> FunctionParameter
  -> Doc ann
prettyParameterFunction
  prettyVar
  prettyTypeVar
  (FunctionParameterWithType {parameter = p}) =
    pText "("
      <> Pretty.nest
        2
        ( Pretty.line
            <> prettyParameter prettyVar prettyTypeVar p
            <> Pretty.line
        )
      <> pText ")"
prettyParameterFunction
  prettyVar
  prettyTypeVar
  (FunctionParameterAlone {parameter = p}) =
    prettyParameter prettyVar prettyTypeVar p


prettyParametersFunction
  :: (ExpressionVariableId -> Doc ann)
  -> (TypeVariableId -> Doc ann)
  -> NonEmpty FunctionParameter
  -> Doc ann
prettyParametersFunction prettyVar prettyTypeVar ps =
  (Pretty.vsep <<< toList)
    ( ( Pretty.group
          <<< prettyParam
      )
        <$> ps
    )
  where
    prettyParam = prettyParameterFunction prettyVar prettyTypeVar


prettyFunction
  :: (ExpressionVariableId -> Doc ann)
  -> (TypeVariableId -> Doc ann)
  -> Function
  -> Doc ann
prettyFunction prettyVar prettyTypeVar (Function' {parameters, body}) =
  Pretty.vsep
    [ pText "\\"
        <> Pretty.nest
          2
          ( Pretty.line
              <> prettyParametersFunction
                prettyVar
                prettyTypeVar
                parameters
          )
    , pText "->"
        <> ( Pretty.group
              <<< Pretty.nest 2
           )
          ( Pretty.line
              <> prettyExpression prettyVar prettyTypeVar body
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


prettyExpression
  :: (ExpressionVariableId -> Doc ann)
  -> (TypeVariableId -> Doc ann)
  -> Expression
  -> Doc ann
prettyExpression prettyVar prettyTypeVar e =
  case e of
    EInt {intValue} -> pretty intValue
    EBool {boolValue} ->
      pText $ if boolValue then "True" else "False"
    Variable {name} -> prettyVar name
    Parens {expression} ->
      Pretty.parens $
        prettyExpression
          prettyVar
          prettyTypeVar
          expression
    EFunction {functionValue} ->
      prettyFunction
        prettyVar
        prettyTypeVar
        functionValue
    Application
      { applicationFunction =
        _function
      , applicationRemain = _arguments
      } ->
        (Pretty.group <<< Pretty.nest 2)
          ( Pretty.line'
              <> prettyArg _function
              <> Pretty.nest
                2
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
                  ( prettyExpression
                      prettyVar
                      prettyTypeVar
                      expr
                  )
              else prettyExpression prettyVar prettyTypeVar expr
    If {condition = _condition, ifTrue = __then, ifFalse = __else} ->
      (Pretty.group <<< Pretty.vsep)
        [ pText "if"
            <> Pretty.nest
              2
              ( Pretty.line
                  <> prettyExpression prettyVar prettyTypeVar _condition
              )
        , pText "then"
            <> Pretty.nest
              2
              ( Pretty.line
                  <> prettyExpression prettyVar prettyTypeVar __then
              )
        , pText "else"
            <> Pretty.nest
              2
              ( Pretty.line
                  <> prettyExpression prettyVar prettyTypeVar __else
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
                          <<< prettyDefinition prettyVar prettyTypeVar
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
                  <> prettyExpression prettyVar prettyTypeVar _in
              )
        ]
    Annotation {expression = _expression, _type} ->
      (Pretty.parens <<< Pretty.group)
        ( prettyExpression prettyVar prettyTypeVar _expression
            <> Pretty.line
            <> Pretty.nest
              2
              ( pText ":"
                  <> Pretty.line
                  <> prettyType prettyTypeVar _type
              )
        )
