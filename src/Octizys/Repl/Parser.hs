{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Repl.Parser (replParserEff) where

import Effectful (Eff, (:>))
import Effectful.Error.Dynamic (Error)
import GHC.Unicode (isAlphaNum)
import Parser
  ( Parser
  , ParserError
  , charParser
  , expressionParser
  , keyword
  , parserToEff
  , sc
  , topParser
  )
import Repl.Ast
  ( ReplCommand (LoadFile, Quit)
  , ReplTop (Command, Define, Evaluate)
  )
import Text.Megaparsec
  ( MonadParsec (takeWhile1P, try)
  , eof
  , (<?>)
  , (<|>)
  )
import Text.Megaparsec.Char (char, string)


-- TODO: we can use char 'q' an factorise to avoid the use of try
-- but is not worth the effort right now.
quitParser :: Parser ReplCommand
quitParser = do
  _ <-
    try (keyword "quit")
      <|> charParser 'q'
      <|> try (keyword "Quit")
      <|> charParser 'Q'
  eof
  return Quit


filePathParser :: Parser String
filePathParser = do
  start <- takeWhile1P (Just "alphanumeric character") isAlphaNum
  extension <- string ".oct" <?> "extension \".oct\""
  pure $ start <> extension


loadParser :: Parser ReplCommand
loadParser = do
  _ <-
    try (keyword "load")
      <|> charParser 'l'
  path <- filePathParser
  eof
  pure $ LoadFile path


commandParser :: Parser ReplCommand
commandParser = do
  _ <- char ':'
  quitParser <|> loadParser


replParser :: Parser ReplTop
replParser = do
  _ <- sc
  ( Command
      <$> (commandParser <* eof)
    )
    <|> try
      ( Define
          <$> (topParser <* eof)
      )
    <|> ( Evaluate
            <$> (expressionParser <* eof)
        )


replParserEff :: Error ParserError :> es => String -> Eff es ReplTop
replParserEff = parserToEff replParser
