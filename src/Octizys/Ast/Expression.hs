{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Octizys.Ast.Expression
  ( ValueInt (ValueInt', value)
  , ValueBool (ValueBool', value)
  , Function (Function', parameters, body, inferType)
  , Value
    ( VInt
    , VBool
    , VFunction
    )
  , Definition (Definition', name, definition, inferType)
  , Variable (Variable', name, inferType)
  , Application (Application', function, argument, inferType)
  , If (If', condition, ifTrue, ifFalse, inferType)
  , Let (Let', definitions, expression, inferType)
  , Annotation (Annotation', expression, _type, inferType)
  , Expression
    ( EVariable
    , EValue
    , EApplication
    , EIf
    , ELet
    , EAnnotation
    )
  , getMonoType
  , getType
  ) where

import Control.Arrow ((<<<))
import Data.Aeson (ToJSON)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Set (difference)
import Data.Text (Text)
import GHC.Generics (Generic, Generically (..))

import Prettyprinter (Doc, Pretty (pretty), (<+>))
import qualified Prettyprinter as Pretty

import Octizys.Ast.Type (Type)
import Octizys.Ast.Type.Basics (TypeValue (BoolType, IntType))
import Octizys.Ast.Type.MonoType (Arrow, MonoType (MonoArrow))
import Octizys.Classes.FreeVariables (FreeVariables (freeVariables))
import Octizys.Classes.From (From (from))
import Octizys.Common.Id (ExpressionVariableId)
import Octizys.Format.Class (Formattable, format)
import qualified Octizys.Format.Config as Format
import qualified Octizys.Format.Utils as Format


newtype ValueInt = ValueInt'
  {value :: Text}
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically ValueInt


instance Formattable ValueInt where
  format _ ValueInt' {value} = pretty @Text value


newtype ValueBool = ValueBool'
  {value :: Bool}
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically ValueBool


instance Formattable ValueBool where
  format _ ValueBool' {value} = pretty value


data Function var = Function'
  { parameters :: NonEmpty (ExpressionVariableId, MonoType var)
  , body :: Expression var
  , inferType :: Arrow var
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (Function var)


instance
  ( Eq var
  , Ord var
  , FreeVariables var (MonoType var)
  , FreeVariables var (Type var)
  )
  => FreeVariables var (Function var)
  where
  freeVariables (Function' {parameters, body, inferType}) =
    difference
      (freeVariables inferType <> freeVariables body)
      (foldMap (\(_, y) -> freeVariables y) parameters)


instance Formattable var => Formattable (Function var) where
  format configuration Function' {parameters, body, inferType} =
    annotateType
      configuration
      ( Pretty.vsep
          [ Format.functionStart
              <> Format.nest
                configuration
                ( Pretty.line
                    <> formatParametersFunction
                      configuration
                      parameters
                )
          , Format.functionBodySeparator
              <> ( Pretty.group
                    <<< Format.nest configuration
                 )
                ( Pretty.line
                    <> formatExpression configuration body
                )
          ]
      )
      (MonoArrow inferType)


data Value var
  = VInt ValueInt
  | VBool ValueBool
  | VFunction (Function var)
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (Value var)


instance From (Value var) ValueInt where
  from = VInt


instance From (Value var) ValueBool where
  from = VBool


instance From (Value var) (Function var) where
  from = VFunction


instance
  ( Eq var
  , Ord var
  , FreeVariables var (MonoType var)
  , FreeVariables var (Type var)
  )
  => FreeVariables var (Value var)
  where
  freeVariables (VFunction i) = freeVariables i
  freeVariables _ = mempty


instance Formattable var => Formattable (Value var) where
  format c (VInt i) = format c i
  format c (VBool i) = format c i
  format c (VFunction i) = format c i


data Definition var = Definition'
  { name :: ExpressionVariableId
  , definition :: Expression var
  , inferType :: Type var
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (Definition var)


instance Formattable var => Formattable (Definition var) where
  format = formatDefinition


instance
  ( Ord var
  , Eq var
  , FreeVariables var (Type var)
  , FreeVariables var (MonoType var)
  )
  => FreeVariables var (Definition var)
  where
  freeVariables d =
    freeVariables d.definition <> freeVariables d.inferType


data Variable var = Variable' {name :: ExpressionVariableId, inferType :: MonoType var}
  deriving (Show, Eq, Ord, Generic, Functor)
  deriving (ToJSON) via Generically (Variable var)


instance Formattable var => Formattable (Variable var) where
  format c (Variable' {name}) = format c name


data Application var = Application'
  { function :: Expression var
  , argument :: Expression var
  , inferType :: MonoType var
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (Application var)


instance Formattable var => Formattable (Application var) where
  format = format


data If var = If'
  { condition :: Expression var
  , ifTrue :: Expression var
  , ifFalse :: Expression var
  , inferType :: MonoType var
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (If var)


data Let var = Let'
  { definitions :: NonEmpty (Definition var)
  , expression :: Expression var
  , inferType :: MonoType var
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (Let var)


data Annotation var = Annotation'
  { expression :: Expression var
  , _type :: MonoType var
  , inferType :: MonoType var
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (Annotation var)


data Expression var
  = EVariable (Variable var)
  | EValue (Value var)
  | EApplication (Application var)
  | EIf (If var)
  | ELet (Let var)
  | EAnnotation (Annotation var)
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (Expression var)


instance From (Expression var) (Value var) where
  from = EValue


instance From (Expression var) (Application var) where
  from = EApplication


instance From (Expression var) (If var) where
  from = EIf


instance From (Expression var) (Let var) where
  from = ELet


instance From (Expression var) (Annotation var) where
  from = EAnnotation


needsParentsInApplication :: Expression var -> Bool
needsParentsInApplication e =
  case e of
    EValue (VFunction {}) -> True
    EValue _ -> False
    EVariable _ -> False
    EApplication _ -> True
    EIf _ -> True
    ELet _ -> True
    EAnnotation _ -> True


instance Formattable var => Formattable (Expression var) where
  format = formatExpression


-- buildValueDefinitionsMap
--   :: Value var -> Map ExpressionVariableId (Expression var)
-- buildValueDefinitionsMap IntValue _ = mempty
-- buildValueDefinitionsMap BoolValue _ = mempty
-- buildValueDefinitionsMap FunctionValue {body} = buildDefinitionsMap body
--
--
-- buildDefinitionsMap
--   :: Expression var -> Map ExpressionVariableId (Expression var)
-- buildDefinitionsMap Variable {} = mempty
-- buildDefinitionsMap EValue {value} = buildValueDefinitionsMap value
-- buildDefinitionsMap Application {applicationFunction, applicationArgument} =
--   Map.union
--     (buildDefinitionsMap applicationFunction)
--     (buildDefinitionsMap applicationArgument)
-- buildDefinitionsMap If {condition, ifTrue, ifFalse} =
--   Map.union
--     (buildDefinitionsMap condition)
--     ( Map.union
--         (buildDefinitionsMap ifTrue)
--         (buildDefinitionsMap ifFalse)
--     )
-- buildDefinitionsMap Let {definitions, expression} =
--   Map.union
--     (buildDefinitionsMap expression)
--     (foldMap buildFromDefinition definitions)
--   where
--     buildFromDefinition
--       :: Definition var -> Map ExpressionVariableId (Expression var)
--     buildFromDefinition Definition' {name, definition} =
--       Map.union (Map.singleton name definition) (buildDefinitionsMap definition)
-- buildDefinitionsMap Annotation {expression} =
--   buildDefinitionsMap expression

instance
  ( Eq var
  , Ord var
  , FreeVariables var (MonoType var)
  , FreeVariables var (Type var)
  )
  => FreeVariables var (Expression var)
  where
  freeVariables (EValue v) = freeVariables v
  freeVariables (EVariable Variable' {inferType}) = freeVariables inferType
  freeVariables (EApplication Application' {inferType, function, argument}) =
    freeVariables inferType
      <> freeVariables function
      <> freeVariables argument
  freeVariables (EIf If' {inferType, condition, ifTrue, ifFalse}) =
    freeVariables inferType
      <> freeVariables ifTrue
      <> freeVariables ifFalse
      <> freeVariables condition
  freeVariables (ELet Let' {inferType, definitions, expression}) =
    freeVariables inferType
      <> freeVariables expression
      <> foldMap freeVariables definitions
  freeVariables (EAnnotation Annotation' {expression, _type, inferType}) =
    freeVariables expression <> freeVariables _type <> freeVariables inferType


getMonoType :: Expression var -> MonoType var
getMonoType (EVariable Variable' {inferType}) = inferType
getMonoType (EValue VInt {}) = from IntType
getMonoType (EValue VBool {}) = from BoolType
getMonoType (EValue (VFunction Function' {inferType})) = from inferType
getMonoType (EApplication Application' {inferType}) = inferType
getMonoType (EIf If' {inferType}) = inferType
getMonoType (ELet Let' {inferType}) = inferType
getMonoType (EAnnotation Annotation' {inferType}) = inferType


getType :: Expression var -> Type var
getType = from <<< getMonoType


-- * Format


formatValue
  :: Formattable var
  => Format.Configuration
  -> Value var
  -> Doc ann
formatValue configuration (VInt v) = format configuration v
formatValue configuration (VBool v) = format configuration v
formatValue configuration (VFunction v) = format configuration v


formatParameterFunction
  :: Formattable var
  => Format.Configuration
  -> (ExpressionVariableId, MonoType var)
  -> Doc ann
formatParameterFunction c (expr, t) =
  Pretty.parens
    ( pretty expr
        <+> ":"
        <+> format c t
    )


formatParametersFunction
  :: Formattable var
  => Format.Configuration
  -> NonEmpty (ExpressionVariableId, MonoType var)
  -> Doc ann
formatParametersFunction c ps =
  (Pretty.vsep <<< toList)
    ( ( Pretty.group
          <<< formatParameterFunction c
      )
        <$> ps
    )


formatDefinition
  :: Formattable var
  => Format.Configuration
  -> Definition var
  -> Doc ann
formatDefinition configuration Definition' {name, definition, inferType} =
  pretty name
    <+> ":"
    <+> format configuration inferType
    <+> "="
    <+> formatExpression configuration definition


annotateType
  :: Formattable var
  => Format.Configuration
  -> Doc ann
  -> MonoType var
  -> Doc ann
annotateType configuration doc t =
  if Format.shouldAstShowTypes configuration
    then
      Pretty.parens
        ( Pretty.group
            ( doc
                <> Format.nest
                  configuration
                  ( Pretty.line'
                      <> Format.text ":"
                      <> format configuration t
                  )
            )
        )
    else doc


formatApplication
  :: Formattable var
  => Format.Configuration
  -> Application var
  -> Doc ann
formatApplication configuration (Application' {function, argument, inferType}) =
  annotateType
    configuration
    ( (Pretty.group <<< Format.nest configuration)
        ( Pretty.line'
            <> prettyArg function
            <> prettyArg argument
        )
    )
    inferType
  where
    prettyArg expr =
      if needsParentsInApplication expr
        then
          Pretty.parens
            ( formatExpression
                configuration
                expr
            )
        else formatExpression configuration expr


formatIf :: Formattable var => Format.Configuration -> If var -> Doc ann
formatIf configuration (If' {condition, ifTrue, ifFalse, inferType}) =
  annotateType
    configuration
    ( (Pretty.group <<< Pretty.vsep)
        [ Format.text "if"
            <> Format.nest
              configuration
              ( Pretty.line
                  <> formatExpression configuration condition
              )
        , Format.text "then"
            <> Format.nest
              configuration
              ( Pretty.line
                  <> formatExpression configuration ifTrue
              )
        , Format.text "else"
            <> Format.nest
              configuration
              ( Pretty.line
                  <> formatExpression configuration ifFalse
              )
        ]
    )
    inferType


formatLet :: Formattable var => Format.Configuration -> Let var -> Doc ann
formatLet configuration (Let' {definitions, expression, inferType}) =
  annotateType
    configuration
    ( Pretty.parens
        ( (Pretty.group <<< Pretty.vsep)
            [ Format.text "let"
                <> Format.nest
                  configuration
                  ( Pretty.line
                      <> (Pretty.vsep <<< toList)
                        ( ( (<> Format.text ";")
                              <<< formatDefinition configuration
                          )
                            <$> definitions
                        )
                  )
            , Format.text
                "in"
                <> Format.nest
                  configuration
                  ( Pretty.line
                      <> formatExpression configuration expression
                  )
            ]
        )
    )
    inferType


formatAnnotation
  :: Formattable var => Format.Configuration -> Annotation var -> Doc ann
formatAnnotation configuration (Annotation' {expression, inferType}) =
  (Pretty.parens <<< Pretty.group)
    ( formatExpression configuration expression
        <> Pretty.line
        <> Format.nest
          configuration
          ( Format.text ":"
              <> Pretty.line
              <> format configuration inferType
          )
    )


formatExpression
  :: Formattable var
  => Format.Configuration
  -> Expression var
  -> Doc ann
formatExpression configuration (EValue v) = formatValue configuration v
formatExpression configuration e@(EVariable v) =
  annotateType
    configuration
    ( format configuration v
    )
    (getMonoType e)
formatExpression configuration (EApplication v) = formatApplication configuration v
formatExpression configuration (EIf v) = formatIf configuration v
formatExpression configuration (ELet v) = formatLet configuration v
formatExpression configuration (EAnnotation v) = formatAnnotation configuration v
