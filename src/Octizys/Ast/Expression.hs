module Octizys.Ast.Expression where

import Control.Arrow ((<<<))
import Data.Foldable (Foldable (fold), foldl')
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Set (Set, difference, fromList)
import Data.Text (Text)
import Effectful.Dispatch.Dynamic (HasCallStack)
import Octizys.Ast.Type (Type, freeVariables)
import Octizys.Cst.Expression (ExpressionVariableId)
import Octizys.Cst.Type (TypeVariableId)
import Prettyprinter (Doc, Pretty (pretty), (<+>))
import qualified Prettyprinter as Pretty


data Definition = Definition'
  { name :: ExpressionVariableId
  , definition :: Expression
  , inferType :: Type
  }
  deriving (Show, Eq, Ord)


definitionFreeTypeVars :: Definition -> Set TypeVariableId
definitionFreeTypeVars d =
  freeTypeVars d.definition <> freeVariables d.inferType


instance Pretty Definition where
  pretty Definition' {name, definition, inferType} =
    pretty name
      <+> ":"
      <+> pretty inferType
      <+> "="
      <+> pretty definition


data Expression
  = EInt {intValue :: Text, inferType :: Type}
  | EBool {boolValue :: Bool, inferType :: Type}
  | Variable {name :: ExpressionVariableId, inferType :: Type}
  | Function
      { parameters :: NonEmpty (ExpressionVariableId, Type)
      , body :: Expression
      , inferType :: Type
      }
  | Application
      { applicationFunction :: Expression
      , applicationArgument :: Expression
      , inferType :: Type
      }
  | If
      { condition :: Expression
      , ifTrue :: Expression
      , ifFalse :: Expression
      , inferType :: Type
      }
  | Let
      { -- The alone info is the semicolon finishing a definition
        definitions :: NonEmpty Definition
      , expression :: Expression
      , inferType :: Type
      }
  | Annotation
      { expression :: Expression
      , _type :: Type
      , inferType :: Type
      }
  deriving (Show, Eq, Ord)


freeTypeVars :: Expression -> Set TypeVariableId
freeTypeVars (EInt {inferType}) = freeVariables inferType
freeTypeVars (EBool {inferType}) = freeVariables inferType
freeTypeVars (Variable {inferType}) = freeVariables inferType
freeTypeVars (Function {inferType, body, parameters}) =
  difference
    (freeVariables inferType <> freeTypeVars body)
    (foldl' (\a (_, y) -> a <> freeVariables y) mempty parameters)
freeTypeVars (Application {inferType, applicationFunction, applicationArgument}) =
  freeVariables inferType
    <> freeTypeVars applicationFunction
    <> freeTypeVars applicationArgument
freeTypeVars (If {inferType, condition, ifTrue, ifFalse}) =
  freeVariables inferType
    <> freeTypeVars ifTrue
    <> freeTypeVars ifFalse
    <> freeTypeVars condition
freeTypeVars (Let {inferType, definitions, expression}) =
  freeVariables inferType
    <> freeTypeVars expression
    <> foldMap definitionFreeTypeVars definitions
freeTypeVars Annotation {expression, _type, inferType} =
  freeTypeVars expression <> freeVariables _type <> freeVariables inferType


pText :: Text -> Doc ann
pText = pretty @Text


prettyParameterFunction
  :: (ExpressionVariableId, Type)
  -> Doc ann
prettyParameterFunction (expr, t) =
  Pretty.parens (pretty expr <+> ":" <+> pretty t)


prettyParametersFunction
  :: NonEmpty (ExpressionVariableId, Type)
  -> Doc ann
prettyParametersFunction ps =
  (Pretty.vsep <<< toList)
    ( ( Pretty.group
          <<< prettyParameterFunction
      )
        <$> ps
    )


needsParentsInApplication :: Expression -> Bool
needsParentsInApplication e =
  case e of
    EInt {} -> False
    EBool {} -> False
    Variable {} -> False
    Function {} -> True
    Application {} -> True
    If {} -> True
    Let {} -> True
    Annotation {} -> True


instance Pretty Expression where
  pretty EInt {intValue} = pretty @Text intValue
  pretty EBool {boolValue} = pretty boolValue
  pretty Variable {name, inferType} =
    Pretty.parens (pretty name <+> ":" <+> pretty inferType)
  pretty Function {parameters, body, inferType} =
    Pretty.parens
      ( Pretty.parens
          ( Pretty.vsep
              [ pText "\\"
                  <> Pretty.nest
                    2
                    ( Pretty.line
                        <> prettyParametersFunction
                          parameters
                    )
              , pText "->"
                  <> ( Pretty.group
                        <<< Pretty.nest 2
                     )
                    ( Pretty.line
                        <> pretty body
                    )
              ]
          )
          <+> ":"
          <+> pretty inferType
      )
  pretty Application {applicationFunction, applicationArgument, inferType} =
    Pretty.parens
      ( Pretty.parens
          ( (Pretty.group <<< Pretty.nest 2)
              ( Pretty.line'
                  <> prettyArg applicationFunction
                  <> prettyArg applicationArgument
              )
          )
          <+> ":"
          <+> pretty inferType
      )
    where
      prettyArg expr =
        if needsParentsInApplication expr
          then
            Pretty.parens
              ( pretty
                  expr
              )
          else pretty expr
  pretty If {condition, ifTrue, ifFalse, inferType} =
    Pretty.parens
      ( Pretty.parens
          ( (Pretty.group <<< Pretty.vsep)
              [ pText "if"
                  <> Pretty.nest
                    2
                    ( Pretty.line
                        <> pretty condition
                    )
              , pText "then"
                  <> Pretty.nest
                    2
                    ( Pretty.line
                        <> pretty ifTrue
                    )
              , pText "else"
                  <> Pretty.nest
                    2
                    ( Pretty.line
                        <> pretty ifFalse
                    )
              ]
          )
          <+> ":"
          <+> pretty inferType
      )
  pretty Let {definitions, expression, inferType} =
    Pretty.parens
      ( Pretty.parens
          ( (Pretty.group <<< Pretty.vsep)
              [ pText "let"
                  <> Pretty.nest
                    2
                    ( Pretty.line
                        <> (Pretty.vsep <<< toList)
                          ( ( (<> pText ";")
                                <<< pretty
                            )
                              <$> definitions
                          )
                    )
              , pText
                  "in"
                  <> Pretty.nest
                    2
                    ( Pretty.line
                        <> pretty expression
                    )
              ]
          )
          <+> ":"
          <+> pretty inferType
      )
  pretty Annotation {expression, inferType} =
    (Pretty.parens <<< Pretty.group)
      ( pretty expression
          <> Pretty.line
          <> Pretty.nest
            2
            ( pText ":"
                <> Pretty.line
                <> pretty inferType
            )
      )


getType :: HasCallStack => Expression -> Type
getType e = e.inferType
