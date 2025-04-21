{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Repl.Parser (replParserEff) where

import Data.Void (Void)
import Effectful (Eff, (:>))
import Effectful.Error.Dynamic (Error)
import Parser (ParserError, parseExpression, parseTop, parserToEff)
import Repl.Ast (ReplCommand (Quit), ReplTop (Command, Define, Evaluate))
import Text.Megaparsec
  ( MonadParsec (try)
  , Parsec
  , empty
  , eof
  , optional
  , (<|>)
  )
import Text.Megaparsec.Char (char, space1)
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void String


-- Espacios opcionales
sc :: Parser ()
sc = L.space space1 empty empty


quitParser :: Parser ReplCommand
quitParser = do
  _ <- char 'q' <|> char 'Q'
  _ <- optional sc
  eof
  return Quit


commandParser :: Parser ReplCommand
commandParser = do
  _ <- char ':'
  quitParser


replParser :: Parser ReplTop
replParser =
  Command
    <$> commandParser
      <|> try (Define <$> parseTop)
      <|> Evaluate
    <$> parseExpression


replParserEff :: Error ParserError :> es => String -> Eff es ReplTop
replParserEff = parserToEff replParser
