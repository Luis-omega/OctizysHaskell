{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Repl.Parser (replParserEff) where

import Effectful (Eff, (:>))
import Effectful.Error.Dynamic (Error)
import Parser
  ( Parser
  , ParserError
  , expressionParser
  , parserToEff
  , topParser
  )
import Repl.Ast (ReplCommand (Quit), ReplTop (Command, Define, Evaluate))
import Text.Megaparsec
  ( MonadParsec (try)
  , empty
  , eof
  , optional
  , (<|>)
  )
import Text.Megaparsec.Char (char, space1)
import qualified Text.Megaparsec.Char.Lexer as L


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
