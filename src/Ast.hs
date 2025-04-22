{-# LANGUAGE RecordWildCards #-}

module Ast
  ( Expression
      ( LiteralExpression,
        Variable,
        Function,
        Application,
        If,
        Let,
        Annotation
      ),
    AstError (IsAkeywordNotVariable),
    Type (IntType, BoolType, Arrow, TypeVar),
    TopItem,
    Context,
    Symbol,
    Literal,
    makeEmptyContext,
    makeSymbol,
    makeBool,
    makeInt,
    makeVariable,
    makeFunction,
    makeApplication,
    makeIf,
    makeLet,
    makeAnnotation,
    makeIntType,
    makeBoolType,
    makeArrow,
    makeTypeVar,
    prettyExpression,
    prettyLiteral,
    prettyType,
    prettySymbol,
    prettyTopItem,
    makeTopItem,
    symbolToString,
    makeVariableFromSymbol,
    ParserType,
    ParserExpression,
    ParserTypeVariable (UserTypeVariable, MachineTypeVariable),
    ParserExpressionVariable (ParserNamedVariable),
    makeMachineTypeVariable,
    makeUserTypeVariable,
    makeParserExpressionVariable,
    makeParserExpressionVariableFromSymbol,
    ParserTopItem,
  )
where

import Control.Arrow ((<<<))
import Data.Bifunctor (Bifunctor (bimap))
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map (empty)
import Text.Megaparsec (ShowErrorComponent)
import Text.Megaparsec.Error (ShowErrorComponent (showErrorComponent))

data AstError
  = CantCreateSymbol String
  | CantCreateVariable String
  | IsAkeywordNotVariable String
  | EmptyParameters
  | EmptyArguments
  | EmptyArrowCodomain
  deriving (Show, Eq, Ord)

instance ShowErrorComponent AstError where
  showErrorComponent = show

newtype Symbol = SymbolC String deriving (Show)

prettySymbol :: Symbol -> String
prettySymbol (SymbolC s) = s

-- TODO: Add logic to check symbols to be real symbols
makeSymbol :: String -> Either AstError Symbol
makeSymbol = pure . SymbolC

symbolToString :: Symbol -> String
symbolToString (SymbolC s) = s

data ParserTypeVariable
  = UserTypeVariable Int
  | MachineTypeVariable Int
  deriving (Show)

makeUserTypeVariable :: Int -> ParserType
makeUserTypeVariable = TypeVar <<< UserTypeVariable

makeMachineTypeVariable :: Int -> ParserType
makeMachineTypeVariable =
  TypeVar <<< MachineTypeVariable

newtype ParserExpressionVariable
  = ParserNamedVariable Symbol
  deriving (Show)

makeParserExpressionVariable ::
  String ->
  Either AstError ParserExpression
makeParserExpressionVariable s =
  makeParserExpressionVariableFromSymbol <$> makeSymbol s

makeParserExpressionVariableFromSymbol ::
  Symbol ->
  ParserExpression
makeParserExpressionVariableFromSymbol =
  Variable <<< ParserNamedVariable

type ParserType = Type ParserTypeVariable

type ParserExpression =
  Expression ParserExpressionVariable ParserTypeVariable

type ParserTopItem =
  TopItem ParserExpressionVariable ParserTypeVariable

data Type vars
  = IntType
  | BoolType
  | Arrow {initial :: Type vars, end :: [Type vars]}
  | TypeVar vars
  deriving (Show)

prettyType :: (Show vars) => Type vars -> String
prettyType t =
  case t of
    IntType -> "int"
    BoolType -> "bool"
    Arrow {initial = _domain, end = _codomain} ->
      "( "
        <> prettyType _domain
        <> " -> "
        <> intercalate
          " -> "
          (prettyType <$> _codomain)
        <> " )"
    TypeVar i -> "_" <> show i

makeIntType :: Type vars
makeIntType = IntType

makeBoolType :: Type vars
makeBoolType = BoolType

makeArrow :: Type vars -> [Type vars] -> Either AstError (Type vars)
makeArrow _ [] = Left EmptyArrowCodomain
makeArrow initial end = Right $ Arrow {..}

makeTypeVar :: vars -> Type vars
makeTypeVar = TypeVar

data Literal = IntLiteral Int | BoolLiteral Bool deriving (Show)

prettyLiteral :: Literal -> String
prettyLiteral l =
  case l of
    IntLiteral i -> show i
    BoolLiteral b -> show b

data Expression vars tvars
  = LiteralExpression Literal
  | Variable vars
  | Function {parameters :: [vars], body :: Expression vars tvars}
  | Application {function :: Expression vars tvars, arguments :: [Expression vars tvars]}
  | If
      { condition :: Expression vars tvars,
        _then :: Expression vars tvars,
        _else :: Expression vars tvars
      }
  | Let {name :: vars, definition :: Expression vars tvars}
  | Annotation {expression :: Expression vars tvars, _type :: Type tvars}
  deriving (Show)

prettyExpression ::
  (Show evars) =>
  (Show tvars) =>
  Expression evars tvars ->
  String
prettyExpression e =
  case e of
    LiteralExpression l -> prettyLiteral l
    -- TODO : if we use proper pretty printing we must correct this use of show
    Variable s -> show s
    Function {parameters = _parameters, body = _body} ->
      "\\ "
        <> concatMap (\s -> show s <> " ") _parameters
        <> "-> "
        <> prettyExpression _body
    Application {function = _function, arguments = _arguments} ->
      "( "
        <> prettyExpression _function
        <> " "
        <> concatMap (\expr -> prettyExpression expr <> " ") _arguments
        <> ")"
    If {condition = _condition, _then = __then, _else = __else} ->
      "if "
        <> prettyExpression _condition
        <> " then "
        <> prettyExpression __then
        <> " else "
        <> prettyExpression __else
    Let {name = _name, definition = _definition} ->
      "let "
        <> show _name
        <> " = "
        <> prettyExpression _definition
    Annotation {expression = _expression, _type = __type} ->
      "( "
        <> prettyExpression _expression
        <> " : "
        <> prettyType __type
        <> " )"

makeBool :: Bool -> Expression evars tvars
makeBool = LiteralExpression . BoolLiteral

makeInt :: Int -> Expression evars tvars
makeInt = LiteralExpression . IntLiteral

makeVariable :: String -> Either AstError (Expression Symbol tvars)
makeVariable s = bimap (\_ -> CantCreateVariable s) Variable $ makeSymbol s

-- TODO: change its name
makeVariableFromSymbol :: evars -> Expression evars tvars
makeVariableFromSymbol = Variable

makeFunction ::
  [evars] ->
  Expression evars tvars ->
  Either AstError (Expression evars tvars)
makeFunction [] _ = Left EmptyParameters
makeFunction parameters body = pure $ Function {..}

makeApplication ::
  Expression evars tvars ->
  [Expression evars tvars] ->
  Either AstError (Expression evars tvars)
makeApplication _ [] = Left EmptyArguments
makeApplication function arguments = pure $ Application {..}

makeIf ::
  Expression evars tvars ->
  Expression evars tvars ->
  Expression evars tvars ->
  Expression evars tvars
makeIf condition _then _else = If {..}

makeLet ::
  evars ->
  Expression evars tvars ->
  Expression evars tvars
makeLet name definition = Let {..}

makeAnnotation ::
  Expression evars tvars ->
  Type tvars ->
  Expression evars tvars
makeAnnotation expression _type = Annotation {..}

data TopItem evars tvars = Definition
  { definition_name :: Symbol,
    definition_type :: Type tvars,
    definition_body :: Expression evars tvars
  }
  deriving (Show)

prettyTopItem ::
  (Show evars) =>
  (Show tvars) =>
  TopItem evars tvars ->
  String
prettyTopItem t =
  prettySymbol (definition_name t)
    <> " : "
    <> prettyType (definition_type t)
    <> " = { "
    <> prettyExpression (definition_body t)
    <> " }"

makeTopItem ::
  Symbol ->
  Type tvars ->
  Expression evars tvars ->
  TopItem evars tvars
makeTopItem
  definition_name
  definition_type
  definition_body = Definition {..}

data Context evars tvars = Context
  { expressions :: Map evars (Type tvars),
    type_variables :: Map tvars (Maybe (Type tvars))
  }
  deriving (Show)

makeEmptyContext :: Context evars tvars
makeEmptyContext =
  Context
    { expressions =
        Data.Map.empty,
      type_variables = Data.Map.empty
    }
