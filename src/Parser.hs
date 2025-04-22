{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Parser
  ( ParserError,
    Parser,
    parseTop,
    parseExpression,
    parserToEff,
    expressionParser,
    topParser,
    uninplemented,
  )
where

import Ast
  ( AstError,
    Expression,
    ParserExpression,
    ParserExpressionVariable (ParserNamedVariable),
    ParserTopItem,
    ParserType,
    Symbol,
    Type,
    makeApplication,
    makeArrow,
    makeBool,
    makeBoolType,
    makeFunction,
    makeIf,
    makeInt,
    makeIntType,
    makeLet,
    makeParserExpressionVariableFromSymbol,
    makeSymbol,
    makeTopItem,
    makeUserTypeVariable,
    symbolToString,
  )
import Control.Arrow ((<<<))
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Functor (void)
import Effectful (Eff, (:>))
import Effectful.Error.Dynamic (Error, throwError)
import Text.Megaparsec
  ( MonadParsec (lookAhead, takeWhile1P, takeWhileP),
    ParseErrorBundle,
    Parsec,
    between,
    customFailure,
    empty,
    many,
    parse,
    some,
    (<?>),
    (<|>),
  )
import qualified Text.Megaparsec.Byte.Lexer as L
import Text.Megaparsec.Char (char, space1)

type ParserError = ParseErrorBundle String AstError

type Parser = Parsec AstError String

uninplemented :: forall a. String -> Parser a
uninplemented s = fail ("Uninplemented " <> s <> " parser")

withPredicate1 :: (a -> Bool) -> String -> Parser a -> Parser a
withPredicate1 predicate errorMsg p = do
  result <- lookAhead p
  if predicate result
    then p
    else fail errorMsg

liftError ::
  forall a e.
  (Ord e) =>
  Parsec e String (Either e a) ->
  Parsec e String a
liftError p = do
  result <- p
  case result of
    Left e -> customFailure e
    Right r -> pure r

-- Espacios opcionales
sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

identifierOrKeyword :: Parser Symbol
identifierOrKeyword = lexeme $ do
  _head <- takeWhile1P (Just "identifier start character") isAlpha
  remain <-
    takeWhileP
      (Just "identifier character")
      (\c -> isAlphaNum c || c == '_')
  let full_string = _head <> remain
  liftError $ pure $ makeSymbol full_string

identifierParser :: Parser Symbol
identifierParser =
  withPredicate1
    ( \s ->
        symbolToString s
          `notElem` [ "if",
                      "then",
                      "else"
                    ]
    )
    "keyword found, expected identifier"
    identifierOrKeyword

charParser :: Char -> Parser ()
charParser = void <<< lexeme <<< char

keyword :: String -> Parser ()
keyword = void <<< symbol

colon :: Parser ()
colon = charParser ':'

semicolon :: Parser ()
semicolon = charParser ';'

equal :: Parser ()
equal = charParser '='

lambdaStart :: Parser ()
lambdaStart = charParser '\\'

rightArrow :: Parser ()
rightArrow = keyword "->"

leftParen :: Parser ()
leftParen = charParser '('

rightParen :: Parser ()
rightParen = charParser ')'

leftBrace :: Parser ()
leftBrace = charParser '{'

rightBrace :: Parser ()
rightBrace = charParser '}'

ifKeyword :: Parser ()
ifKeyword = keyword "if"

thenKeyword :: Parser ()
thenKeyword = keyword "then"

elseKeyword :: Parser ()
elseKeyword = keyword "else"

letKeyword :: Parser ()
letKeyword = keyword "let"

parens :: Parser a -> Parser a
parens = between leftParen rightParen

braces :: Parser a -> Parser a
braces = between leftBrace rightBrace

simpleIntParser :: Parser Int
simpleIntParser =
  lexeme
    ( read
        <$> takeWhile1P (Just "int digit") isDigit
    )

typeIntParser :: Parser (Type tvar)
typeIntParser = makeIntType <$ symbol "int"

typeBoolParser :: Parser (Type tvar)
typeBoolParser = makeBoolType <$ symbol "int"

typeConstantParser :: Parser (Type tvar)
typeConstantParser = typeIntParser <|> typeBoolParser

typeVariableParser :: Parser ParserType
typeVariableParser =
  ( char '_'
      >> (makeUserTypeVariable <$> simpleIntParser)
  )
    <?> "a type variable"

typeAtom :: Parser ParserType
typeAtom =
  typeConstantParser
    <|> typeVariableParser
    <|> parens typeParser

typeArrowParser :: Parser ParserType
typeArrowParser = do
  initial <- typeAtom
  remain <- some (symbol "->" >> typeAtom)
  (liftError <<< pure <<< makeArrow initial) remain

typeParser :: Parser ParserType
typeParser = typeArrowParser

boolParser :: Parser ParserExpression
boolParser =
  (makeBool True <$ symbol "true")
    <|> (makeBool False <$ symbol "false")

intParser :: Parser (Expression evars tvars)
intParser =
  lexeme
    ( do
        _head <- takeWhile1P (Just "digit") isDigit
        others <- takeWhileP (Just "digit or _") (\c -> isDigit c || c == '_')
        (pure <<< makeInt <<< read) (_head <> others)
    )
    <?> "valid integer"

variableParser :: Parser ParserExpression
variableParser =
  makeParserExpressionVariableFromSymbol <$> identifierParser

functionParser :: Parser ParserExpression
functionParser = do
  lambdaStart
  parameters <-
    some
      ( ParserNamedVariable
          <$> identifierParser
      )
  rightArrow
  liftError
    (makeFunction parameters <$> expressionParser)

atomExpressionParser :: Parser ParserExpression
atomExpressionParser =
  boolParser
    <|> intParser
    <|> functionParser
    <|> parensExpressionParser
    <|> ifParser
    <|> letParser
    -- Keep it at the end, it prevents the capture
    -- of keywords by variableParser
    -- Maybe we should check inside variableParser
    -- but this is a cheap trick
    <|> variableParser

parensExpressionParser :: Parser ParserExpression
parensExpressionParser = parens expressionParser

ifParser :: Parser ParserExpression
ifParser = do
  _ <- ifKeyword
  condition <- expressionParser
  _ <- thenKeyword
  _then <- expressionParser
  _ <- elseKeyword
  makeIf condition _then <$> expressionParser

letParser :: Parser ParserExpression
letParser = do
  _ <- letKeyword
  name <- ParserNamedVariable <$> identifierParser
  _ <- equal
  expr <- expressionParser
  _ <- semicolon
  pure $ makeLet name expr

applicationParser :: Parser ParserExpression
applicationParser = do
  function <- atomExpressionParser
  arguments <- many atomExpressionParser
  case arguments of
    [] -> pure function
    _ -> case makeApplication function arguments of
      Left e -> customFailure e
      Right x -> pure x

expressionParser :: Parser ParserExpression
expressionParser = applicationParser

parserToEff :: (Error ParserError :> es) => Parser a -> String -> Eff es a
parserToEff p s =
  -- TODO : remove this harcode of "repl"
  case parse p "repl" s of
    Left e -> throwError e
    Right a -> pure a

topParser :: Parser ParserTopItem
topParser = do
  name <- identifierParser
  _type <-
    colon >> typeParser
  equal
  braces
    ( makeTopItem name _type <$> expressionParser
    )

parseExpression ::
  (Error ParserError :> es) => String -> Eff es ParserExpression
parseExpression = parserToEff expressionParser

parseTop :: (Error ParserError :> es) => String -> Eff es ParserTopItem
parseTop = parserToEff topParser
