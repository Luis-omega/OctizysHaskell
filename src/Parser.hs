{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Parser (ParserError, parseTop, parseExpression, parserToEff, parseTopEff) where

import Ast (Expression, TopItem)
import Data.Void (Void)
import Effectful (Eff, (:>))
import Effectful.Error.Dynamic (Error, throwError)
import Text.Megaparsec (ParseErrorBundle, Parsec, parse)

type ParserError = ParseErrorBundle String Void

type Parser = Parsec Void String

parserToEff :: (Error ParserError :> es) => Parser a -> String -> Eff es a
parserToEff p s =
  -- TODO : remove this harcode of "repl"
  case parse p "repl" s of
    Left e -> throwError e
    Right a -> pure a

parseExpression :: Parser Expression
parseExpression = fail "Uninplemented parser for expressions"

parseTop :: Parser TopItem
parseTop = fail "Uninplemented parser for top"

-- parseExpressionEff :: (Error ParserError :> es) => String -> Eff es Expression
-- parseExpressionEff s = parseTopEff parseExpression s

parseTopEff :: (Error ParserError :> es) => String -> Eff es TopItem
parseTopEff = parserToEff parseTop
