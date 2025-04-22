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
  , functionParameters
  , functionBody
  , AstError (IsAkeywordNotVariable)
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
  , makeVariableFromSymbol
  , ParserType
  , ParserExpression
  , ParserTypeVariable (UserTypeVariable, MachineTypeVariable)
  , ParserExpressionVariable (ParserNamedVariable)
  , makeMachineTypeVariable
  , makeUserTypeVariable
  , makeParserExpressionVariable
  , makeParserExpressionVariableFromSymbol
  , ParserTopItem
  , getNewInt
  , applicationArguments
  , applicationFunction
  , ifCondition
  , ifThen
  , ifElse
  , letDefinition
  , letName
  , annotationType
  , annotationExpression
  , topItemName
  , topItemBody
  , topItemType
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


newtype Symbol = SymbolC String deriving (Show, Eq, Ord)


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
  deriving (Show, Eq, Ord)


makeParserExpressionVariable
  :: String
  -> Either AstError ParserExpression
makeParserExpressionVariable s =
  makeParserExpressionVariableFromSymbol <$> makeSymbol s


makeParserExpressionVariableFromSymbol
  :: Symbol
  -> ParserExpression
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


prettyType :: Show vars => Type vars -> String
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
  | Let {letName :: vars, letDefinition :: Expression vars tvars}
  | Annotation
      { annotationExpression :: Expression vars tvars
      , annotationType :: Type tvars
      }
  deriving (Show)


prettyExpression
  :: Show evars
  => Show tvars
  => Expression evars tvars
  -> String
prettyExpression e =
  case e of
    LiteralExpression l -> prettyLiteral l
    -- TODO : if we use proper pretty printing we must correct this use of show
    Variable s -> show s
    Function {functionParameters = _parameters, functionBody = _body} ->
      "\\ "
        <> concatMap (\s -> show s <> " ") _parameters
        <> "-> "
        <> prettyExpression _body
    Application
      { applicationFunction =
        _function
      , applicationArguments = _arguments
      } ->
        "( "
          <> prettyExpression _function
          <> " "
          <> concatMap (\expr -> prettyExpression expr <> " ") _arguments
          <> ")"
    If {ifCondition = _condition, ifThen = __then, ifElse = __else} ->
      "if "
        <> prettyExpression _condition
        <> " then "
        <> prettyExpression __then
        <> " else "
        <> prettyExpression __else
    Let {letName = _name, letDefinition = _definition} ->
      "let "
        <> show _name
        <> " = "
        <> prettyExpression _definition
    Annotation {annotationExpression = _expression, annotationType = __type} ->
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


makeLet
  :: evars
  -> Expression evars tvars
  -> Expression evars tvars
makeLet letName letDefinition = Let {..}


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


prettyTopItem
  :: Show evars
  => Show tvars
  => TopItem evars tvars
  -> String
prettyTopItem t =
  prettySymbol (topItemName t)
    <> " : "
    <> prettyType (topItemType t)
    <> " = { "
    <> prettyExpression (topItemBody t)
    <> " }"


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


data Context evars tvars = Context
  { expressions :: Map evars (Type tvars)
  , type_variables :: Map tvars (Maybe (Type tvars))
  , variable_counter :: Int
  }
  deriving (Show)


makeEmptyContext :: Context evars tvars
makeEmptyContext =
  Context
    { expressions =
        Data.Map.empty
    , type_variables = Data.Map.empty
    , variable_counter = 0
    }


getNewInt :: Context evars tvars -> (Int, Context evars tvars)
getNewInt
  ( Context
      { expressions =
        _expressions
      , type_variables = _type_variables
      , variable_counter = _variable_counter
      }
    ) =
    ( _variable_counter
    , Context
        { expressions =
            _expressions
        , type_variables = _type_variables
        , variable_counter = _variable_counter + 1
        }
    )
