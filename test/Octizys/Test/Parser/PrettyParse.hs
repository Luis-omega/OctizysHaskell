{-# LANGUAGE ScopedTypeVariables #-}

module Octizys.Test.Parser.PrettyParse (tests) where

import Control.Arrow ((<<<))
import Octizys.Parser
  ( Parser,
    eof,
    errorBundlePretty,
    expressionParser,
    moduleParser,
    testParser,
    topParser,
    typeParser,
  )
import Octizys.Repl.Repl (render)
import Prettyprinter (Pretty)
import Test.Hspec

-- | This module is on charge of testing:
-- "(render <<< pretty <<< parser)"
tests :: SpecWith ()
tests = do
  describe "expression parser" $ do
    testPositiveExpression "1"
    testPositiveExpression "if True then 1 else 0"
    testPositiveExpression "let x = 42; in x 3 4"
    testPositiveExpression "\\x -> x y z w"
    testPositiveExpression "f a (b c) d"
    testPositiveExpression fixtureFactorial

  describe "type parser" $ do
    testPositiveType "int"
    testPositiveType "bool"

    testPositiveType "int -> int"

    testPositiveType "int -> int -> int"

    testPositiveType "(int -> int) -> int"
    testPositiveType "int -> (int -> int)"

    testPositiveType "(int -> int) -> (bool -> bool)"

  describe "definition parser" $ do
    testPositiveTopItem
      "fact : int -> int = { if lt n 2 then 1 else mul n (fact (minus n 1)) }"
    testPositiveTopItem
      "not : bool -> bool = { \\x -> if x then False else True }"
    testPositiveTopItem
      "and : bool -> bool -> bool = { \\x y -> if x then y else False }"
    testPositiveTopItem "isZero : int -> bool = { \\n -> eq n 0 }"
    testPositiveTopItem
      "choose : bool -> int -> int -> int = { \\b x y -> if b then x else y }"

  describe "module parser" $ do
    testPositiveFile "test/Examples/factorial.oct"

cleanText :: String -> String
cleanText = filter (`notElem` [' ', '\n', '\t', '\r'])

makePositiveTest ::
  forall a.
  (Pretty a) =>
  Parser a ->
  String ->
  SpecWith ()
makePositiveTest p s =
  it s $ do
    let parsed = testParser p s
    case parsed of
      Right r -> cleanText (render r) `shouldBe` cleanText s
      Left e -> (expectationFailure <<< errorBundlePretty) e

testPositiveExpression :: String -> SpecWith ()
testPositiveExpression =
  makePositiveTest (expressionParser <* eof)

testPositiveType :: String -> SpecWith ()
testPositiveType =
  makePositiveTest (typeParser <* eof)

testPositiveTopItem :: String -> SpecWith ()
testPositiveTopItem =
  makePositiveTest (topParser <* eof)

makePositiveFileTest ::
  forall a.
  (Pretty a) =>
  Parser [a] ->
  String ->
  String ->
  SpecWith ()
makePositiveFileTest p path s =
  it path $ do
    let parsed = testParser p s
    case parsed of
      Right r ->
        (cleanText <<< concat) (render <$> r)
          `shouldBe` cleanText s
      Left e -> (expectationFailure <<< errorBundlePretty) e

testPositiveFile :: String -> SpecWith ()
testPositiveFile path = do
  content <- runIO $ readFile path
  makePositiveFileTest (moduleParser <* eof) path content

fixtureFactorial :: String
fixtureFactorial =
  "let fact = \\ n -> if lt n 2 then 1 else mul n (fact (minus n 1 )); in fact 5"
