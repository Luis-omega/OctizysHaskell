{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Parser
  ( ParserError
  , parseTop
  , parseExpression
  , parserToEff
  , parseTopEff
  , parseExpressionEff
  )
where

import Ast (Expression, Symbol, TopItem, Type, makeTopItem, symbolToString)
import Data.Functor (void)
import Data.Void (Void)
import Effectful (Eff, (:>))
import Effectful.Error.Dynamic (Error, throwError)
import Text.Megaparsec (ParseErrorBundle, Parsec, empty, parse, (<?>))
import qualified Text.Megaparsec.Byte.Lexer as L
import Text.Megaparsec.Char (char, space1)


type ParserError = ParseErrorBundle String Void


type Parser = Parsec Void String


uninplemented :: forall a. String -> Parser a
uninplemented s = fail ("Uninplemented " <> s <> " parser")


-- Espacios opcionales
sc :: Parser ()
sc = L.space space1 empty empty


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc


symbol :: String -> Parser String
symbol = L.symbol sc


identifierParser :: Parser Symbol
identifierParser = uninplemented "identifier"


colon :: Parser ()
colon = void $ lexeme (char ':')


semicolon :: Parser ()
semicolon = void $ lexeme (char ';')


equal :: Parser ()
equal = void $ lexeme (char '=')


typeParser :: Parser Type
typeParser = uninplemented "type"


expressionParser :: Parser Expression
expressionParser = uninplemented "expression"


parserToEff :: Error ParserError :> es => Parser a -> String -> Eff es a
parserToEff p s =
  -- TODO : remove this harcode of "repl"
  case parse p "repl" s of
    Left e -> throwError e
    Right a -> pure a


parseExpression :: Parser Expression
parseExpression = expressionParser


parseTop :: Parser TopItem
parseTop = do
  name <- identifierParser
  _type <-
    colon >> typeParser
  semicolon
  _ <-
    symbol (symbolToString name)
      <?> ( "A declaration of a type must be followed by the definition, expected name: "
              <> symbolToString name
          )
  equal
  makeTopItem name _type <$> expressionParser


parseExpressionEff
  :: Error ParserError :> es => String -> Eff es Expression
parseExpressionEff = parserToEff parseExpression


parseTopEff :: Error ParserError :> es => String -> Eff es TopItem
parseTopEff = parserToEff parseTop
