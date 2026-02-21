module Octizys.FrontEnd.Cst.Expression
  ( Parameter (ParameterAlone, ParameterWithType, name, _type, colon)
  , getParameterType
  , getAnnotatedParameters
  , SchemeStart (SchemeStart', _forall, typeArguments, dot)
  , DefinitionTypeAnnotation
    ( DefinitionTypeAnnotation'
    , colon
    , schemeStart
    , parameters
    , outputType
    )
  , Definition
    ( Definition'
    , name
    , equal
    , definition
    , _type
    )
  , Expression
    ( EInt
    , EBool
    , EVariable
    , EParens
    , EFunction
    , EApplication
    , EIf
    , ELet
    , EAnnotation
    )
  , Parameters
    ( Parameters'
    , initParameter
    , otherParameters
    , bodySeparator
    )
  , IntExpression (IntExpression', info, value)
  , BoolExpression (BoolExpression', info, value)
  , Variable (Variable', info, name)
  , Parens (Parens', lparen, rparen, expression)
  , Function (Function', start, parameters, body)
  , Application (Application', function, remain)
  , If (If', _if, condition, _then, ifTrue, _else, ifFalse)
  , Let (Let', _let, definitions, _in, expression)
  , Annotation (Annotation', expression, colon, _type)
  )
where

import Control.Arrow ((<<<))
import Data.Foldable (Foldable (foldl'))
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Set as Set
import Data.Text (Text)
import Octizys.Classes.FreeVariables (FreeVariables (freeVariables))
import Octizys.FrontEnd.Cst.SourceInfo (SourceInfo)
import Octizys.FrontEnd.Cst.Type (Type)

import Data.Aeson (ToJSON)
import qualified Data.List.NonEmpty as NonEmpty
import GHC.Generics (Generic, Generically (..))
import Octizys.Classes.From (From (from))
import Octizys.Format.Class (Formattable (format))
import qualified Octizys.Format.Config as Format
import qualified Octizys.Format.Utils as Format
import Prettyprinter (Doc, Pretty (pretty), (<+>))
import qualified Prettyprinter as Pretty


-- | The set of parameters
data Parameter evar tvar
  = ParameterAlone {name :: (SourceInfo, evar)}
  | ParameterWithType
      { name :: (SourceInfo, evar)
      , colon :: SourceInfo
      , _type :: Type tvar
      }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (Parameter evar tvar)


instance Ord evar => FreeVariables evar (Parameter evar tvar) where
  freeVariables ParameterAlone {name} = Set.singleton (snd name)
  freeVariables ParameterWithType {name} = Set.singleton (snd name)


instance (Formattable evar, Formattable tvar) => Formattable (Parameter evar tvar) where
  format = formatParameter


getParameterType :: Parameter evar tvar -> Maybe (Type tvar)
getParameterType (ParameterAlone _) = Nothing
getParameterType (ParameterWithType _ _ c) = Just c


data Parameters evar tvar = Parameters'
  { initParameter :: Parameter evar tvar
  , otherParameters :: [(SourceInfo, Parameter evar tvar)]
  , bodySeparator :: SourceInfo
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (Parameters evar tvar)


instance (Formattable evar, Formattable tvar) => Formattable (Parameters evar tvar) where
  format = formatParameters


instance Ord evar => FreeVariables evar (Parameters evar tvar) where
  freeVariables p =
    foldl'
      (<>)
      (freeVariables p.initParameter)
      ( (freeVariables <<< snd) <$> p.otherParameters
      )


getAnnotatedParameters
  :: Parameters evar tvar -> NonEmpty (evar, Maybe (Type tvar))
getAnnotatedParameters ps =
  (\x -> (snd x.name, getParameterType x))
    <$> (ps.initParameter :| (snd <$> ps.otherParameters))


-- | forall a b c .
data SchemeStart tvar = SchemeStart'
  { _forall :: SourceInfo
  , typeArguments :: NonEmpty (SourceInfo, tvar)
  , dot :: SourceInfo
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (SchemeStart tvar)


instance Formattable tvar => Formattable (SchemeStart tvar) where
  format = formatSchemeStart


data DefinitionTypeAnnotation evar tvar = DefinitionTypeAnnotation'
  { colon :: SourceInfo
  , schemeStart :: Maybe (SchemeStart tvar)
  , parameters :: Maybe (Parameters evar tvar)
  , outputType :: Type tvar
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (DefinitionTypeAnnotation evar tvar)


instance
  (Formattable evar, Formattable tvar)
  => Formattable (DefinitionTypeAnnotation evar tvar)
  where
  format = formatDefinitionTypeAnnotation


-- | Either a Let definition or a Top level definition
data Definition evar tvar = Definition'
  { name :: (SourceInfo, evar)
  , _type :: Maybe (DefinitionTypeAnnotation evar tvar)
  , equal :: SourceInfo
  , definition :: Expression evar tvar
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (Definition evar tvar)


instance
  (Formattable evar, Formattable tvar)
  => Formattable (Definition evar tvar)
  where
  format = formatDefinition


data IntExpression = IntExpression' {info :: SourceInfo, value :: Text}
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically IntExpression


instance Formattable IntExpression where
  format _ (IntExpression' {value}) = pretty value


data BoolExpression = BoolExpression' {info :: SourceInfo, value :: Bool}
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically BoolExpression


instance Formattable BoolExpression where
  format _ (BoolExpression' {value}) = pretty value


data Variable evar = Variable' {info :: SourceInfo, name :: evar}
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (Variable evar)


instance Formattable evar => Formattable (Variable evar) where
  format configuration (Variable' {name}) = format configuration name


data Parens evar tvar = Parens'
  { lparen :: SourceInfo
  , expression :: Expression evar tvar
  , rparen :: SourceInfo
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (Parens evar tvar)


instance
  (Formattable tvar, Formattable evar)
  => Formattable (Parens evar tvar)
  where
  format configuration (Parens' {expression}) =
    Pretty.parens
      (format configuration expression)


data Function evar tvar = Function'
  { start :: SourceInfo
  , parameters :: Parameters evar tvar
  , body :: Expression evar tvar
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (Function evar tvar)


instance
  (Formattable tvar, Formattable evar)
  => Formattable (Function evar tvar)
  where
  format = formatFunction


data Application evar tvar = Application'
  { function :: Expression evar tvar
  , remain :: NonEmpty (Expression evar tvar)
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (Application evar tvar)


instance
  (Formattable tvar, Formattable evar)
  => Formattable (Application evar tvar)
  where
  format = formatApplication


data If evar tvar = If'
  { _if :: SourceInfo
  , condition :: Expression evar tvar
  , _then :: SourceInfo
  , ifTrue :: Expression evar tvar
  , _else :: SourceInfo
  , ifFalse :: Expression evar tvar
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (If evar tvar)


instance
  (Formattable tvar, Formattable evar)
  => Formattable (If evar tvar)
  where
  format = formatIf


data Let evar tvar = Let'
  { _let :: SourceInfo
  , -- The alone info is the semicolon finishing a definition
    definitions :: NonEmpty (Definition evar tvar, SourceInfo)
  , _in :: SourceInfo
  , expression :: Expression evar tvar
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (Let evar tvar)


instance
  (Formattable tvar, Formattable evar)
  => Formattable (Let evar tvar)
  where
  format = formatLet


data Annotation evar tvar = Annotation'
  { expression :: Expression evar tvar
  , colon :: SourceInfo
  , _type :: Type tvar
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (Annotation evar tvar)


instance
  (Formattable evar, Formattable tvar)
  => Formattable (Annotation evar tvar)
  where
  format = formatAnnotation


data Expression evar tvar
  = EInt IntExpression
  | EBool BoolExpression
  | EVariable (Variable evar)
  | EParens (Parens evar tvar)
  | EFunction (Function evar tvar)
  | EApplication (Application evar tvar)
  | EIf (If evar tvar)
  | ELet (Let evar tvar)
  | EAnnotation (Annotation evar tvar)
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (Expression evar tvar)


instance
  (Formattable evar, Formattable tvar)
  => Formattable (Expression evar tvar)
  where
  format = formatExpression


instance From (Expression evar tvar) IntExpression where
  from = EInt


instance From (Expression evar tvar) BoolExpression where
  from = EBool


instance From (Expression evar tvar) (Variable evar) where
  from = EVariable


instance From (Expression evar tvar) (Parens evar tvar) where
  from = EParens


instance From (Expression evar tvar) (Function evar tvar) where
  from = EFunction


instance From (Expression evar tvar) (Application evar tvar) where
  from = EApplication


instance From (Expression evar tvar) (If evar tvar) where
  from = EIf


instance From (Expression evar tvar) (Let evar tvar) where
  from = ELet


instance From (Expression evar tvar) (Annotation evar tvar) where
  from = EAnnotation


-- * Format


formatParameter
  :: Formattable tvar
  => Formattable evar
  => Format.Configuration
  -> Parameter evar tvar
  -> Doc ann
formatParameter
  cfg
  (ParameterAlone {name = (_, v)}) = format cfg v
formatParameter
  configuration
  (ParameterWithType {name = (_, v), _type = t}) =
    format configuration v
      <> ( Pretty.group
            <<< Format.nest configuration
         )
        ( Pretty.line
            <> Format.text
              ":"
            <+> format configuration t
        )


formatParameters
  :: Formattable tvar
  => Formattable evar
  => Format.Configuration
  -> Parameters evar tvar
  -> Doc ann
formatParameters
  configuration
  (Parameters' {initParameter, otherParameters}) =
    Format.formatListItemsWith
      configuration
      (\_ x -> x)
      Pretty.comma
      ( formatParameter
          configuration
          initParameter
          : ( prettyArg
                <$> otherParameters
            )
      )
    where
      prettyArg (_, p) =
        Pretty.group
          ( formatParameter
              configuration
              p
          )


formatSchemeStart
  :: Formattable tvar
  => Format.Configuration
  -> SchemeStart tvar
  -> Doc ann
formatSchemeStart configuration SchemeStart' {typeArguments} =
  Format.text "forall"
    <> Pretty.line
    <> ( Pretty.group
          <<< Format.nest
            configuration
       )
      ( Pretty.fillSep
          ((format configuration <<< snd) <$> NonEmpty.toList typeArguments)
      )
    <> Pretty.line
    <> pretty '.'


formatDefinitionTypeAnnotation
  :: Formattable tvar
  => Formattable evar
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
            (Pretty.group <<< Format.nest configuration)
              ( Pretty.line
                  <> formatParameters
                    configuration
                    ps
              )
          Nothing -> mempty
      outType =
        case parameters of
          Nothing ->
            (Pretty.group <<< Format.nest configuration)
              ( Pretty.line <> format configuration outputType
              )
          Just _ ->
            Pretty.group
              ( Format.functionSeparator
                  <> Pretty.line
                  <> format configuration outputType
              )
     in
      pretty ':'
        <> scheme
        <> pars
        <> Format.nest configuration outType


formatDefinition
  :: Formattable evar
  => Formattable tvar
  => Format.Configuration
  -> Definition evar tvar
  -> Doc ann
formatDefinition
  configuration
  (Definition' {name = (_, v), _type, definition}) =
    let n = format configuration v
        def =
          ( Pretty.line
              <> formatExpression configuration definition
          )
     in n
          <> maybe
            mempty
            (formatDefinitionTypeAnnotation configuration)
            _type
          <> (Pretty.group <<< Format.nest configuration)
            ( Pretty.line
                <> pretty '='
                <> Pretty.group def
            )


needsParentsInApplication :: Expression evar tvar -> Bool
needsParentsInApplication e =
  case e of
    EInt {} -> False
    EBool {} -> False
    EVariable {} -> False
    EParens {} -> False
    EFunction {} -> True
    EApplication {} -> True
    EIf {} -> True
    ELet {} -> True
    EAnnotation {} -> True


formatFunction
  :: (Formattable tvar, Formattable evar)
  => Format.Configuration
  -> Function evar tvar
  -> Doc ann
formatFunction configuration (Function' {parameters, body}) =
  Pretty.group
    ( Format.functionStart
        <> Pretty.group
          ( Format.nest
              configuration
              ( Pretty.line
                  <> formatParameters
                    configuration
                    parameters
              )
          )
    )
    <> Pretty.line
    <> Pretty.group
      ( Format.functionBodySeparator
          <> ( Pretty.group
                <<< Format.nest configuration
             )
            ( Pretty.line
                <> formatExpression
                  configuration
                  body
            )
      )


formatApplication
  :: (Formattable evar, Formattable tvar)
  => Format.Configuration
  -> Application evar tvar
  -> Doc ann
formatApplication
  configuration
  ( Application'
      { function =
        _function
      , remain = _arguments
      }
    ) =
    (Pretty.group <<< Format.nest configuration)
      ( Pretty.line'
          <> prettyArg _function
          <> Format.nest
            configuration
            ( Pretty.line
                <> (Pretty.vsep <<< NonEmpty.toList)
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


formatIf
  :: (Formattable evar, Formattable tvar)
  => Format.Configuration
  -> If evar tvar
  -> Doc ann
formatIf
  configuration
  (If' {condition = _condition, ifTrue = __then, ifFalse = __else}) =
    (Pretty.group <<< Pretty.vsep)
      [ Format.text "if"
          <> Format.nest
            configuration
            ( Pretty.line
                <> formatExpression
                  configuration
                  _condition
            )
      , Format.text "then"
          <> Format.nest
            configuration
            ( Pretty.line
                <> formatExpression
                  configuration
                  __then
            )
      , Format.text "else"
          <> Format.nest
            configuration
            ( Pretty.line
                <> formatExpression
                  configuration
                  __else
            )
      ]


formatLet
  :: (Formattable evar, Formattable tvar)
  => Format.Configuration
  -> Let evar tvar
  -> Doc ann
formatLet
  configuration
  (Let' {definitions, expression = _in}) =
    (Pretty.group <<< Pretty.vsep)
      [ Format.text "let"
          <> Format.nest
            configuration
            ( Pretty.line
                <> (Pretty.vsep <<< NonEmpty.toList)
                  ( ( (<> Format.text ";")
                        <<< formatDefinition configuration
                        <<< fst
                    )
                      <$> definitions
                  )
            )
      , Format.text
          "in"
          <> Format.nest
            configuration
            ( Pretty.line
                <> formatExpression
                  configuration
                  _in
            )
      ]


formatAnnotation
  :: (Formattable evar, Formattable tvar)
  => Format.Configuration
  -> Annotation evar tvar
  -> Doc ann
formatAnnotation
  configuration
  (Annotation' {expression = _expression, _type}) =
    (Pretty.parens <<< Pretty.group)
      ( formatExpression
          configuration
          _expression
          <> Pretty.line
          <> Format.nest
            configuration
            ( Format.text ":"
                <> Pretty.line
                <> format configuration _type
            )
      )


formatExpression
  :: Formattable evar
  => Formattable tvar
  => Format.Configuration
  -> Expression evar tvar
  -> Doc ann
formatExpression configuration e =
  case e of
    EInt v -> format configuration v
    EBool v ->
      format
        configuration
        v
    EVariable v -> format configuration v
    EParens v -> format configuration v
    EFunction v -> format configuration v
    EApplication v -> format configuration v
    EIf v -> format configuration v
    ELet v -> format configuration v
    EAnnotation v -> format configuration v
