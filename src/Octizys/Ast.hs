{-# LANGUAGE RecordWildCards #-}

module Octizys.Ast
  ( Expression
      ( LiteralExpression
      , Variable
      , Function
      , Application
      , If
      , Let
      , Annotation
      )
  , functionParameters
  , functionBody
  , AstError (IsAkeywordNotVariable)
  , Type (IntType, BoolType, Arrow, TypeVar)
  , arrowInitial
  , arrowEnd
  , TopItem
  , Symbol
  , Literal (IntLiteral, BoolLiteral)
  , makeSymbol
  , makeBool
  , makeInt
  , makeVariable
  , makeFunction
  , makeApplication
  , makeIf
  , makeLet
  , makeLetDefinition
  , makeAnnotation
  , makeIntType
  , LetDefinition (LetDefinitionC)
  , makeBoolType
  , makeArrow
  , makeTypeVar
  , makeTopItem
  , symbolToString
  , makeVariableFromSymbol
  , applicationArguments
  , applicationFunction
  , ifCondition
  , ifThen
  , ifElse
  , letDefinition
  , letDefinitions
  , letName
  , letIn
  , annotationType
  , annotationExpression
  , topItemName
  , topItemBody
  , topItemType
  )
where

import Control.Arrow ((<<<))
import Data.Bifunctor (Bifunctor (bimap))
import Prettyprinter (Pretty (pretty), (<+>))
import Prettyprinter qualified as Pretty
import Text.Megaparsec (ShowErrorComponent)
import Text.Megaparsec.Error (ShowErrorComponent (showErrorComponent))


data AstError
  = CantCreateSymbol String
  | CantCreateVariable String
  | IsAkeywordNotVariable String
  | EmptyParameters
  | EmptyArguments
  | EmptyArrowCodomain
  | EmptyLetDefinitions
  deriving (Show, Eq, Ord)


instance ShowErrorComponent AstError where
  showErrorComponent = show


newtype Symbol = SymbolC String deriving (Show, Eq, Ord)


instance Pretty Symbol where
  pretty (SymbolC c) = pretty c


-- TODO: Add logic to check symbols to be real symbols
makeSymbol :: String -> Either AstError Symbol
makeSymbol = pure . SymbolC


symbolToString :: Symbol -> String
symbolToString (SymbolC s) = s


data Type vars
  = IntType
  | BoolType
  | Arrow {arrowInitial :: Type vars, arrowEnd :: [Type vars]}
  | TypeVar vars
  deriving (Show)


needsParentsInArrow :: Type vars -> Bool
needsParentsInArrow t =
  case t of
    IntType -> False
    BoolType -> False
    Arrow {} -> True
    TypeVar _ -> False


instance Pretty vars => Pretty (Type vars) where
  pretty t =
    case t of
      IntType -> pretty "int"
      BoolType -> pretty "bool"
      Arrow {arrowInitial = _domain, arrowEnd = _codomain} ->
        (Pretty.group <<< Pretty.nest 2)
          ( Pretty.line'
              <> Pretty.concatWith
                (\l r -> l <> Pretty.line <> pretty "->" <> r)
                ( prettyArg
                    <$> (_domain : _codomain)
                )
          )
        where
          prettyArg ty =
            if needsParentsInArrow ty
              then Pretty.parens (pretty ty)
              else pretty ty
      TypeVar v -> pretty v


makeIntType :: Type vars
makeIntType = IntType


makeBoolType :: Type vars
makeBoolType = BoolType


makeArrow :: Type vars -> [Type vars] -> Either AstError (Type vars)
makeArrow _ [] = Left EmptyArrowCodomain
makeArrow arrowInitial arrowEnd = Right $ Arrow {..}


makeTypeVar :: vars -> Type vars
makeTypeVar = TypeVar


data Literal = IntLiteral String | BoolLiteral Bool deriving (Show)


instance Pretty Literal where
  pretty l =
    case l of
      IntLiteral i -> pretty i
      BoolLiteral b -> pretty b


data LetDefinition vars tvars = LetDefinitionC
  { letName :: vars
  , letDefinition :: Expression vars tvars
  }
  deriving (Show)


instance (Pretty vars, Pretty tvars) => Pretty (LetDefinition vars tvars) where
  pretty LetDefinitionC {letName = _name, letDefinition = _definition} =
    Pretty.group
      ( pretty _name
          <> Pretty.nest
            2
            ( Pretty.line
                <> pretty "="
                <> Pretty.nest 2 (Pretty.line <> pretty _definition)
                <> pretty ";"
            )
      )


data Expression vars tvars
  = LiteralExpression Literal
  | Variable vars
  | Function
      {functionParameters :: [vars], functionBody :: Expression vars tvars}
  | Application
      { applicationFunction :: Expression vars tvars
      , applicationArguments :: [Expression vars tvars]
      }
  | If
      { ifCondition :: Expression vars tvars
      , ifThen :: Expression vars tvars
      , ifElse :: Expression vars tvars
      }
  | Let
      { letDefinitions :: [LetDefinition vars tvars]
      , letIn :: Expression vars tvars
      }
  | Annotation
      { annotationExpression :: Expression vars tvars
      , annotationType :: Type tvars
      }
  deriving (Show)


needsParentsInApplication :: Expression evars tvars -> Bool
needsParentsInApplication e =
  case e of
    LiteralExpression _ -> False
    Variable _ -> False
    Function {} -> True
    Application {} -> True
    If {} -> True
    Let {} -> True
    Annotation {} -> True


instance (Pretty evars, Pretty tvars) => Pretty (Expression evars tvars) where
  pretty e =
    case e of
      LiteralExpression l -> pretty l
      Variable s -> pretty s
      Function {functionParameters = _parameters, functionBody = _body} ->
        (Pretty.group <<< Pretty.vsep)
          [ pretty "\\"
              <> ( Pretty.group
                    <<< Pretty.nest 2
                 )
                (Pretty.line <> Pretty.vsep (pretty <$> _parameters))
          , pretty "->"
              <> Pretty.line
              <> Pretty.nest 2 (pretty _body)
          ]
      Application
        { applicationFunction =
          _function
        , applicationArguments = _arguments
        } ->
          (Pretty.group <<< Pretty.nest 2)
            ( Pretty.line'
                <> prettyArg _function
                <> Pretty.nest
                  2
                  ( Pretty.line
                      <> Pretty.vsep (prettyArg <$> _arguments)
                  )
            )
          where
            prettyArg expr =
              if needsParentsInApplication expr
                then Pretty.parens (pretty expr)
                else pretty expr
      If {ifCondition = _condition, ifThen = __then, ifElse = __else} ->
        (Pretty.group <<< Pretty.vsep)
          [ pretty "if" <> Pretty.nest 2 (Pretty.line <> pretty _condition)
          , pretty "then" <> Pretty.nest 2 (Pretty.line <> pretty __then)
          , pretty "else" <> Pretty.nest 2 (Pretty.line <> pretty __else)
          ]
      Let {letDefinitions = definitions, letIn = _in} ->
        (Pretty.group <<< Pretty.vsep)
          [ pretty "let"
              <> Pretty.nest
                2
                ( Pretty.line
                    <> Pretty.vsep (pretty <$> definitions)
                )
          , pretty
              "in"
              <> Pretty.nest 2 (Pretty.line <> pretty _in)
          ]
      Annotation {annotationExpression = _expression, annotationType = __type} ->
        (Pretty.parens <<< Pretty.group)
          ( pretty _expression
              <> Pretty.line
              <> Pretty.nest
                2
                ( pretty ":"
                    <> Pretty.line
                    <> pretty __type
                )
          )


makeBool :: Bool -> Expression evars tvars
makeBool = LiteralExpression . BoolLiteral


makeInt :: String -> Expression evars tvars
makeInt = LiteralExpression . IntLiteral


makeVariable :: String -> Either AstError (Expression Symbol tvars)
makeVariable s = bimap (\_ -> CantCreateVariable s) Variable $ makeSymbol s


-- TODO: change its name
makeVariableFromSymbol :: evars -> Expression evars tvars
makeVariableFromSymbol = Variable


makeFunction
  :: [evars]
  -> Expression evars tvars
  -> Either AstError (Expression evars tvars)
makeFunction [] _ = Left EmptyParameters
makeFunction functionParameters functionBody = pure $ Function {..}


makeApplication
  :: Expression evars tvars
  -> [Expression evars tvars]
  -> Either AstError (Expression evars tvars)
makeApplication _ [] = Left EmptyArguments
makeApplication applicationFunction applicationArguments = pure $ Application {..}


makeIf
  :: Expression evars tvars
  -> Expression evars tvars
  -> Expression evars tvars
  -> Expression evars tvars
makeIf ifCondition ifThen ifElse = If {..}


makeLetDefinition
  :: evars
  -> Expression evars tvars
  -> LetDefinition evars tvars
makeLetDefinition letName letDefinition = LetDefinitionC {..}


makeLet
  :: [LetDefinition evars tvars]
  -> Expression evars tvars
  -> Either AstError (Expression evars tvars)
makeLet letDefinitions letIn =
  case letDefinitions of
    [] -> Left EmptyLetDefinitions
    _ -> pure Let {..}


makeAnnotation
  :: Expression evars tvars
  -> Type tvars
  -> Expression evars tvars
makeAnnotation annotationExpression annotationType = Annotation {..}


data TopItem evars tvars = Definition
  { topItemName :: Symbol
  , topItemType :: Type tvars
  , topItemBody :: Expression evars tvars
  }
  deriving (Show)


instance (Pretty evars, Pretty tvars) => Pretty (TopItem evars tvars) where
  pretty t =
    Pretty.group
      ( pretty (topItemName t)
          <+> pretty ":"
          <> (Pretty.group <<< Pretty.nest 2)
            ( Pretty.line
                <> pretty (topItemType t)
            )
          <> Pretty.group
            ( Pretty.line
                <> pretty "="
                <+> pretty "{"
                <> Pretty.nest
                  2
                  ( Pretty.line
                      <> pretty (topItemBody t)
                  )
                <> Pretty.line
                <> pretty "}"
            )
      )


makeTopItem
  :: Symbol
  -> Type tvars
  -> Expression evars tvars
  -> TopItem evars tvars
makeTopItem
  topItemName
  topItemType
  topItemBody =
    Definition {..}
