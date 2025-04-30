{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Octizys.Test.Parser.PrettyParse (tests) where

import Control.Arrow ((<<<))
import Octizys.Parser.Type
  ( OctizysParseError
  , comment
  , token
  )
import Test.Hspec

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import qualified Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Effectful (Eff, runPureEff)
import Effectful.Error.Static (Error)
import Effectful.State.Static.Local (State)
import Octizys.Cst.Expression (ExpressionVariableId)
import Octizys.Cst.InfoId (InfoId)
import Octizys.Cst.Type (TypeVariableId)
import Octizys.Effects.Generator.Effect (Generator, IntGenerator)
import Octizys.Effects.Generator.Interpreter (IntGeneratorState)
import Octizys.Effects.Parser.Backend
  ( ParserError
  , ParserState
  , prettyParserError
  )
import Octizys.Effects.Parser.Combinators (eof, text)
import Octizys.Effects.Parser.Effect (Parser)
import Octizys.Effects.Parser.Interpreter (runFullParser)
import Octizys.Effects.SymbolResolution.Effect (SymbolResolution)
import Octizys.Effects.SymbolResolution.Interpreter
  ( SourceExpressionVariableInfo
  , SourceInfo
  , SourceTypeVariableInfo
  , SymbolResolutionState
  , initialSymbolResolutionState
  , runSymbolResolutionFull
  )
import Octizys.Pretty.Comment (prettyComment)
import Prettyprinter
  ( Doc
  , Pretty (pretty)
  , defaultLayoutOptions
  , layoutPretty
  )
import qualified Prettyprinter as Pretty
import qualified Prettyprinter.Render.String


type P a =
  Eff
    ( Parser OctizysParseError
        : State ParserState
        : Error (ParserError OctizysParseError)
        : SymbolResolution
        : State SymbolResolutionState
        : '[]
    )
    a


runParser
  :: Eff
      ( Parser OctizysParseError
          : State ParserState
          : Error (ParserError OctizysParseError)
          : SymbolResolution
          : State SymbolResolutionState
          : '[]
      )
      a
  -> Text
  -> Either (ParserError OctizysParseError) a
runParser p t =
  runPureEff $
    fst
      <$> runSymbolResolutionFull
        initialSymbolResolutionState
        (runFullParser t p)


renderError :: ParserError OctizysParseError -> String
renderError =
  render
    <<< prettyParserError pretty (Just ('t' :| "est"))


render :: Doc ann -> String
render =
  Prettyprinter.Render.String.renderString
    <<< Prettyprinter.layoutPretty Prettyprinter.defaultLayoutOptions


shouldParse
  :: Eq a
  => (a -> Doc ann)
  -> Either (ParserError OctizysParseError) a
  -> String
  -> Expectation
shouldParse _ (Left e) expected = do
  expectationFailure (renderError e <> "\n" <> "Expected:" <> expected)
shouldParse toDoc (Right result) expected =
  let prettyResult = render (toDoc result)
   in if prettyResult == expected
        then pure ()
        else
          expectationFailure
            ( render
                ( pretty @String "Expected:"
                    <> pretty expected
                    <> Pretty.line
                    <> "Got:"
                    <> toDoc result
                )
            )


{- | This module is on charge of testing:
"(render <<< pretty <<< parser)"
-}
tests :: SpecWith ()
tests = do
  describe "type parser" $ do
    makePositiveTest
      "line comment"
      "-- hola mundo\n"
      (Just "-- hola mundo")
      (comment <* eof @OctizysParseError)
      prettyComment
    makePositiveTest
      "block comment"
      "{- hola mundo\n como estas?\n he?\n bye!-}"
      Nothing
      comment
      prettyComment
    -- TODO: test offsets!
    makePositiveTest
      "let comments"
      "{- hola mundo\n como estas?\n he?\n bye!-} let -- hi!"
      (Just "(let, 0)")
      (let p = token (text @OctizysParseError "let") in p)
      pretty


--      (\(a,source)-> pretty a <> pretty source)

makePositiveTest
  :: Eq a
  => String
  -> Text
  -> Maybe String
  -> P a
  -> (a -> Doc ann)
  -> SpecWith ()
makePositiveTest
  testName
  input
  maybeExpect
  parser
  prettier = do
    it testName $ do
      let result = runParser parser input
      let expected :: String =
            Data.Maybe.fromMaybe (Text.unpack input) maybeExpect
      shouldParse prettier result expected

--  describe "expression parser" $ do
--    testPositiveExpression "1"
--    testPositiveExpression "if True then 1 else 0"
--    testPositiveExpression "let x = 42; in x 3 4"
--    testPositiveExpression "\\x -> x y z w"
--    testPositiveExpression "f a (b c) d"
--    testPositiveExpression fixtureFactorial
--
--  describe "type parser" $ do
--    testPositiveType "int"
--    testPositiveType "bool"
--
--    testPositiveType "int -> int"
--
--    testPositiveType "int -> int -> int"
--
--    testPositiveType "(int -> int) -> int"
--    testPositiveType "int -> (int -> int)"
--
--    testPositiveType "(int -> int) -> (bool -> bool)"
--
--  describe "definition parser" $ do
--    testPositiveTopItem
--      "fact : int -> int = { if lt n 2 then 1 else mul n (fact (minus n 1)) }"
--    testPositiveTopItem
--      "not : bool -> bool = { \\x -> if x then False else True }"
--    testPositiveTopItem
--      "and : bool -> bool -> bool = { \\x y -> if x then y else False }"
--    testPositiveTopItem "isZero : int -> bool = { \\n -> eq n 0 }"
--    testPositiveTopItem
--      "choose : bool -> int -> int -> int = { \\b x y -> if b then x else y }"
--
--  describe "module parser" $ do
--    testPositiveFile "test/Examples/factorial.oct"
--
--
-- cleanText :: String -> String
-- cleanText = filter (`notElem` [' ', '\n', '\t', '\r'])
--
--
-- makePositiveTest
--  :: forall a
--   . Pretty a
--  => Parser a
--  -> String
--  -> SpecWith ()
-- makePositiveTest p s =
--  it s $ do
--    let parsed = testParser p s
--    case parsed of
--      Right r -> cleanText (render r) `shouldBe` cleanText s
--      Left e -> (expectationFailure <<< errorBundlePretty) e
--
--
-- testPositiveExpression :: String -> SpecWith ()
-- testPositiveExpression =
--  makePositiveTest (expressionParser <* eof)
--
--
-- testPositiveType :: String -> SpecWith ()
-- testPositiveType =
--  makePositiveTest (typeParser <* eof)
--
--
-- testPositiveTopItem :: String -> SpecWith ()
-- testPositiveTopItem =
--  makePositiveTest (topParser <* eof)
--
--
-- makePositiveFileTest
--  :: forall a
--   . Pretty a
--  => Parser [a]
--  -> String
--  -> String
--  -> SpecWith ()
-- makePositiveFileTest p path s =
--  it path $ do
--    let parsed = testParser p s
--    case parsed of
--      Right r ->
--        (cleanText <<< concat) (render <$> r)
--          `shouldBe` cleanText s
--      Left e -> (expectationFailure <<< errorBundlePretty) e
--
--
-- testPositiveFile :: String -> SpecWith ()
-- testPositiveFile path = do
--  content <- runIO $ readFile path
--  makePositiveFileTest (moduleParser <* eof) path content
--
--
-- fixtureFactorial :: String
-- fixtureFactorial =
--  "let fact = \\ n -> if lt n 2 then 1 else mul n (fact (minus n 1 )); in fact 5"
