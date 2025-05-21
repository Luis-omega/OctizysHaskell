module Octizys.Ast.Expression where

import Control.Arrow ((<<<))
import Data.Foldable (Foldable (fold), foldl')
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set, difference, fromList)
import Data.Text (Text)
import Debug.Trace (trace)
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
  pretty = prettyDefinition pretty


prettyDefinition
  :: (ExpressionVariableId -> Doc ann)
  -> Definition
  -> Doc ann
prettyDefinition prettyVar Definition' {name, definition, inferType} =
  prettyVar name
    <+> ":"
    <+> pretty inferType
    <+> "="
    <+> prettyExpression prettyVar definition


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
  :: (ExpressionVariableId -> Doc ann)
  -> (ExpressionVariableId, Type)
  -> Doc ann
prettyParameterFunction prettyVar (expr, t) =
  Pretty.parens (prettyVar expr <+> ":" <+> pretty t)


prettyParametersFunction
  :: (ExpressionVariableId -> Doc ann)
  -> NonEmpty (ExpressionVariableId, Type)
  -> Doc ann
prettyParametersFunction prettyVar ps =
  (Pretty.vsep <<< toList)
    ( ( Pretty.group
          <<< prettyParameterFunction prettyVar
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
  pretty = prettyExpression pretty


prettyExpressionWithDic
  :: forall a ann
   . Map ExpressionVariableId a
  -> (a -> Doc ann)
  -> Expression
  -> Doc ann
prettyExpressionWithDic mp toDoc = prettyExpression go
  where
    go :: (ExpressionVariableId -> Doc ann)
    go eid =
      maybe
        (pretty @Text "NoFound[" <> pretty eid <> pretty ']')
        toDoc
        (Map.lookup eid mp)


prettyExpression
  :: (ExpressionVariableId -> Doc ann) -> Expression -> Doc ann
prettyExpression _ EInt {intValue} = pretty @Text intValue
prettyExpression _ EBool {boolValue} = pretty boolValue
prettyExpression prettyVar Variable {name, inferType} =
  Pretty.parens (prettyVar name <+> ":" <+> pretty inferType)
prettyExpression prettyVar Function {parameters, body, inferType} =
  Pretty.parens
    ( Pretty.parens
        ( Pretty.vsep
            [ pText "\\"
                <> Pretty.nest
                  2
                  ( Pretty.line
                      <> prettyParametersFunction
                        prettyVar
                        parameters
                  )
            , pText "->"
                <> ( Pretty.group
                      <<< Pretty.nest 2
                   )
                  ( Pretty.line
                      <> prettyExpression prettyVar body
                  )
            ]
        )
        <+> ":"
        <+> pretty inferType
    )
prettyExpression prettyVar Application {applicationFunction, applicationArgument, inferType} =
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
            ( prettyExpression
                prettyVar
                expr
            )
        else prettyExpression prettyVar expr
prettyExpression prettyVar If {condition, ifTrue, ifFalse, inferType} =
  Pretty.parens
    ( Pretty.parens
        ( (Pretty.group <<< Pretty.vsep)
            [ pText "if"
                <> Pretty.nest
                  2
                  ( Pretty.line
                      <> prettyExpression prettyVar condition
                  )
            , pText "then"
                <> Pretty.nest
                  2
                  ( Pretty.line
                      <> prettyExpression prettyVar ifTrue
                  )
            , pText "else"
                <> Pretty.nest
                  2
                  ( Pretty.line
                      <> prettyExpression prettyVar ifFalse
                  )
            ]
        )
        <+> ":"
        <+> pretty inferType
    )
prettyExpression prettyVar Let {definitions, expression, inferType} =
  Pretty.parens
    ( Pretty.parens
        ( (Pretty.group <<< Pretty.vsep)
            [ pText "let"
                <> Pretty.nest
                  2
                  ( Pretty.line
                      <> (Pretty.vsep <<< toList)
                        ( ( (<> pText ";")
                              <<< prettyDefinition prettyVar
                          )
                            <$> definitions
                        )
                  )
            , pText
                "in"
                <> Pretty.nest
                  2
                  ( Pretty.line
                      <> prettyExpression prettyVar expression
                  )
            ]
        )
        <+> ":"
        <+> pretty inferType
    )
prettyExpression prettyVar Annotation {expression, inferType} =
  (Pretty.parens <<< Pretty.group)
    ( prettyExpression prettyVar expression
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
