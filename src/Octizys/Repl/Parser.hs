{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Octizys.Repl.Parser (replParser) where

import Effectful (Eff, (:>))
import GHC.Unicode (isAlphaNum)
import Octizys.Effects.Parser.Combinators
  ( char
  , eof
  , takeWhile1P
  , text
  , try
  , (<?>)
  , (<|>)
  )

import Control.Arrow ((<<<))
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as Text
import Octizys.Effects.Parser.Effect (Parser)
import Octizys.Effects.SymbolResolution.Effect (SymbolResolution)
import Octizys.Parser.Common (OctizysParseError, skipSimpleSpaces)
import Octizys.Parser.Expression (parseExpression)
import Octizys.Parser.TopItem (parseModule)
import Octizys.Repl.Ast
  ( ReplCommand (LoadFile, Quit)
  , ReplTop (Command, Define, Evaluate)
  )


-- TODO: remove the spaces after!
keyword
  :: Parser OctizysParseError :> es
  => NonEmpty Char
  -> Eff es Text
keyword = text <<< Text.pack <<< NonEmpty.toList


-- TODO: we can use char 'q' an factorise to avoid the use of try
-- but is not worth the effort right now.
quitParser
  :: Parser OctizysParseError :> es
  => Eff es ReplCommand
quitParser = do
  _ <-
    try (keyword ('q' :| "uit"))
      <|> keyword ('q' :| [])
      <|> try (keyword ('Q' :| "uit"))
      <|> keyword ('Q' :| [])
  eof
  return Quit


filePathParser
  :: Parser OctizysParseError :> es
  => Eff es Text
filePathParser = do
  start <- takeWhile1P (Just "alphanumeric character") isAlphaNum
  extension <- text ".oct" <?> ('e' :| "xtension \".oct\"")
  pure $ start <> extension


loadParser
  :: Parser OctizysParseError :> es
  => Eff es ReplCommand
loadParser = do
  _ <-
    try (keyword ('l' :| "oad")) <|> keyword ('l' :| [])
  path <- filePathParser
  eof
  pure $ LoadFile path


commandParser
  :: Parser OctizysParseError :> es
  => Eff es ReplCommand
commandParser = do
  _ <- char ':'
  quitParser <|> loadParser


replParser
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es ReplTop
replParser = do
  _ <- skipSimpleSpaces
  ( Command
      <$> (commandParser <* eof)
    )
    --    <|> try
    --      ( Define
    --          <$> (parseModule <* eof)
    --      )
    <|> ( Evaluate
            <$> (parseExpression <* eof)
        )
