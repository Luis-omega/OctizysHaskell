{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Repl.Parser (parseString, ReplParserError) where

import Effectful (Eff, (:>))
import Effectful.Error.Dynamic (Error)
import Parser (ParserError, parseExpressionOrTop)
import Repl.Ast (ReplCommand (Quit), ReplTop (Command, Define, Evaluate))

data ReplParserError = UndefinedReplParser deriving (Show)

parseString :: (Error ReplParserError :> es, Error ParserError :> es) => String -> Eff es ReplTop
parseString s =
  case s of
    -- TODO: Introduce a proper parser
    ":q" -> pure $ Command Quit
    _ -> do
      result <- parseExpressionOrTop s
      pure $ case result of
        Left e -> Evaluate e
        Right d -> Define d
