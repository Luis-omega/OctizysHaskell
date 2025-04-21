{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Parser (parseString, ParserError, parseExpression, parseExpressionOrTop) where

import Ast (Expression, TopItem)
import Effectful (Eff, (:>))
import Effectful.Error.Dynamic (Error, throwError)

data ParserError = UninplementedParser String deriving (Show)

parseString :: (Error ParserError :> es) => String -> Eff es [TopItem]
parseString _ = throwError $ UninplementedParser "parseString"

parseExpression :: (Error ParserError :> es) => String -> Eff es Expression
parseExpression _ = throwError $ UninplementedParser "parseExpression"

parseExpressionOrTop :: (Error ParserError :> es) => String -> Eff es (Either Expression TopItem)
parseExpressionOrTop _ = throwError $ UninplementedParser "parseExpressionOrTop"
