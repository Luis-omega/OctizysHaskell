module Octizys.Pretty.Expression where

import Control.Arrow ((<<<))
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Text (Text)
import Octizys.Cst.Expression
  ( Definition (Definition', definition, name, parameters)
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
  , Parameter (Parameter', name, _type)
  , Parameters (Parameters', remain, start)
  )
import Octizys.Cst.InfoId (InfoId)
import Octizys.Cst.Type (TypeVariableId)
import qualified Octizys.Cst.Type as Type
import Octizys.Pretty.Type (needsParentsInArrow, prettyType)
import Prettyprinter (Doc, Pretty (pretty), (<+>))
import qualified Prettyprinter as Pretty


pText :: Text -> Doc ann
pText = pretty @Text


prettyParameter
  :: (ExpressionVariableId -> Doc ann)
  -> (TypeVariableId -> Doc ann)
  -> Parameter
  -> Doc ann
prettyParameter prettyVar prettyTypeVar (Parameter' {name = (_, v), _type}) =
  case _type of
    Just (_, t) ->
      prettyVar v
        <+> pText ":"
        <> Pretty.nest
          2
          ( Pretty.line
              <> if needsParentsInArrow t
                then Pretty.parens (prettyType prettyTypeVar t)
                else prettyType prettyTypeVar t
          )
    Nothing -> prettyVar v


prettyParameters
  :: (ExpressionVariableId -> Doc ann)
  -> (TypeVariableId -> Doc ann)
  -> Parameters
  -> Doc ann
prettyParameters prettyVar prettyTypeVar (Parameters' {start, remain}) =
  (Pretty.nest 2 <<< Pretty.vsep)
    ( Pretty.group
        ( prettyParameter
            prettyVar
            prettyTypeVar
            start
        )
        : ( prettyArg
              <$> remain
          )
    )
  where
    prettyArg (_, p) =
      pText "->"
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
  (Definition' {name = (_, v), parameters, definition}) =
    let n = prettyVar v
        pars =
          case parameters of
            Just (_, ps) ->
              pText ""
                <+> pText ":"
                <> (Pretty.nest 2 <<< Pretty.group)
                  ( Pretty.line
                      <> prettyParameters
                        prettyVar
                        prettyTypeVar
                        ps
                  )
            Nothing -> mempty
        def =
          (Pretty.group <<< Pretty.nest 2)
            ( Pretty.line
                <> prettyExpression prettyVar prettyTypeVar definition
            )
     in n
          <> pars
          <> ( Pretty.line
                <> pText "="
                <> def
             )


prettyParameterFunction
  :: (ExpressionVariableId -> Doc ann)
  -> (TypeVariableId -> Doc ann)
  -> Parameter
  -> Doc ann
prettyParameterFunction
  prettyVar
  prettyTypeVar
  (Parameter' {name = (_, v), _type}) =
    case _type of
      Just (_, t) ->
        let pt =
              prettyVar v
                <+> pText ":"
                <> Pretty.nest
                  2
                  ( Pretty.line
                      <> prettyType prettyTypeVar t
                  )
         in case t of
              Type.Parens {} -> Pretty.parens pt
              _ -> pt
      Nothing -> prettyVar v


prettyParametersFunction
  :: (ExpressionVariableId -> Doc ann)
  -> (TypeVariableId -> Doc ann)
  -> NonEmpty (Either (InfoId, Parameter, InfoId) Parameter)
  -> Doc ann
prettyParametersFunction prettyVar prettyTypeVar ps =
  (Pretty.vsep <<< toList)
    ( ( Pretty.group
          <<< either prettyParens prettyParam
      )
        <$> ps
    )
  where
    prettyParam = prettyParameterFunction prettyVar prettyTypeVar
    prettyParens (_, p, _) =
      -- TODO: is it fine to do this? maybe a line in ")" at least?
      pText "(" <+> prettyParam p <+> pText ")"


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
      pText $ if boolValue then "true" else "false"
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
