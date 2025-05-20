module Octizys.Ast.Expression where

import Control.Arrow ((<<<))
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Text (Text)
import Effectful.Dispatch.Dynamic (HasCallStack)
import Octizys.Ast.Type (Type)
import Octizys.Cst.Expression (ExpressionVariableId)
import Prettyprinter (Doc, Pretty (pretty), (<+>))
import qualified Prettyprinter as Pretty


data Definition = Definition'
  { name :: ExpressionVariableId
  , definition :: Expression
  , inferType :: Type
  }
  deriving (Show, Eq, Ord)


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
