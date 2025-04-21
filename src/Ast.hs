{-# LANGUAGE RecordWildCards #-}

module Ast
  ( Expression
      ( LiteralExpression
      , Variable
      , Function
      , Application
      , If
      , Let
      , Annotation
      )
  , Type (IntType, BoolType, Arrow, TypeVar)
  , TopItem
  , Context
  , Symbol
  , Literal
  , makeEmptyContext
  , makeSymbol
  , makeBool
  , makeInt
  , makeVariable
  , makeFunction
  , makeApplication
  , makeIf
  , makeLet
  , makeAnnotation
  , makeIntType
  , makeBoolType
  , makeArrow
  , makeTypeVar
  , prettyExpression
  , prettyLiteral
  , prettyType
  , prettySymbol
  , prettyTopItem
  , makeTopItem
  , symbolToString
  )
where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Map (Map)
import qualified Data.Map (empty)


data AstError
  = CantCreateSymbol String
  | CantCreateVariable String
  | EmptyParameters
  | EmptyArguments
  deriving (Show)


newtype Symbol = SymbolC String deriving (Show)


prettySymbol :: Symbol -> String
prettySymbol (SymbolC s) = s


-- TODO: Add logic to check symbols to be real symbols
makeSymbol :: String -> Either AstError Symbol
makeSymbol = pure . SymbolC


symbolToString :: Symbol -> String
symbolToString (SymbolC s) = s


data Type
  = IntType
  | BoolType
  | Arrow {domain :: Type, codomain :: Type}
  | TypeVar Int
  deriving (Show)


prettyType :: Type -> String
prettyType t =
  case t of
    IntType -> "int"
    BoolType -> "bool"
    Arrow {domain = _domain, codomain = _codomain} -> "( " <> prettyType _domain <> " -> " <> prettyType _codomain <> " )"
    TypeVar i -> "_" <> show i


makeIntType :: Type
makeIntType = IntType


makeBoolType :: Type
makeBoolType = BoolType


makeArrow :: Type -> Type -> Type
makeArrow domain codomain = Arrow {..}


makeTypeVar :: Int -> Type
makeTypeVar = TypeVar


data Literal = IntLiteral Int | BoolLiteral Bool deriving (Show)


prettyLiteral :: Literal -> String
prettyLiteral l =
  case l of
    IntLiteral i -> show i
    BoolLiteral b -> show b


data Expression
  = LiteralExpression Literal
  | Variable Symbol
  | Function {parameters :: [Symbol], body :: Expression}
  | Application {function :: Expression, arguments :: [Expression]}
  | If {condition :: Expression, _then :: Expression, _else :: Expression}
  | Let {name :: Symbol, definition :: Expression}
  | Annotation {expression :: Expression, _type :: Type}
  deriving (Show)


prettyExpression :: Expression -> String
prettyExpression e =
  case e of
    LiteralExpression l -> prettyLiteral l
    Variable s -> prettySymbol s
    Function {parameters = _parameters, body = _body} ->
      "\\ "
        <> concatMap (\s -> prettySymbol s <> " ") _parameters
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
    Let {name = _name, definition = _definition} -> "let " <> prettySymbol _name <> " = " <> prettyExpression _definition
    Annotation {expression = _expression, _type = __type} ->
      "( " <> prettyExpression _expression <> " : " <> prettyType __type <> " )"


makeBool :: Bool -> Expression
makeBool = LiteralExpression . BoolLiteral


makeInt :: Int -> Expression
makeInt = LiteralExpression . IntLiteral


makeVariable :: String -> Either AstError Expression
makeVariable s = bimap (\_ -> CantCreateVariable s) Variable $ makeSymbol s


makeFunction :: [Symbol] -> Expression -> Either AstError Expression
makeFunction [] _ = Left EmptyParameters
makeFunction parameters body = pure $ Function {..}


makeApplication :: Expression -> [Expression] -> Either AstError Expression
makeApplication _ [] = Left EmptyArguments
makeApplication function arguments = pure $ Application {..}


makeIf :: Expression -> Expression -> Expression -> Expression
makeIf condition _then _else = If {..}


makeLet :: Symbol -> Expression -> Expression
makeLet name definition = Let {..}


makeAnnotation :: Expression -> Type -> Expression
makeAnnotation expression _type = Annotation {..}


data TopItem = Definition
  { definition_name :: Symbol
  , definition_type :: Type
  , definition_body :: Expression
  }
  deriving (Show)


prettyTopItem :: TopItem -> String
prettyTopItem t =
  prettySymbol (definition_name t)
    <> " : "
    <> prettyType (definition_type t)
    <> " = { "
    <> prettyExpression (definition_body t)
    <> " }"


makeTopItem :: Symbol -> Type -> Expression -> TopItem
makeTopItem definition_name definition_type definition_body = Definition {..}


data Context = Context
  {expressions :: Map Symbol Type, type_variables :: Map Int (Maybe Type)}
  deriving (Show)


makeEmptyContext :: Context
makeEmptyContext = Context {expressions = Data.Map.empty, type_variables = Data.Map.empty}
