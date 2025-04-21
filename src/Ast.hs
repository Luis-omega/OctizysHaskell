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
    Type (IntType, BoolType, Arrow, TypeVar),
    TopItem,
    Context,
    Symbol,
    Literal,
    makeEmptyContext,
  )
where

import Data.Map (Map)
import qualified Data.Map (empty)

newtype Symbol = SymbolC String deriving (Show)

data Literal = IntLiteral Int | BoolLiteral Int deriving (Show)

data Expression
  = LiteralExpression Literal
  | Variable Symbol
  | Function {parameters :: [Symbol], body :: Expression}
  | Application {function :: Expression, arguments :: [Expression]}
  | If {condition :: Expression, _if :: Expression, _else :: Expression}
  | Let {name :: Symbol, definition :: Expression}
  | Annotation {expression :: Expression, _type :: Type}
  deriving (Show)

data Type
  = IntType
  | BoolType
  | Arrow {domain :: Type, codomain :: Type}
  | TypeVar Int
  deriving (Show)

data TopItem = Definition
  { definition_name :: Symbol,
    definition_type :: Type,
    definition_body :: Expression
  }
  deriving (Show)

data Context = Context
  {expressions :: Map Symbol Type, type_variables :: Map Int (Maybe Type)}
  deriving (Show)

makeEmptyContext :: Context
makeEmptyContext = Context {expressions = Data.Map.empty, type_variables = Data.Map.empty}
