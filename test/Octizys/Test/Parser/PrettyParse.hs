{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Octizys.Test.Parser.PrettyParse (tests) where

import Control.Arrow ((<<<))
import Octizys.Parser.Common
  ( OctizysParseError
  , comment
  )
import Test.Hspec

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Effectful (Eff, runPureEff)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.State.Static.Local (State)
import Octizys.Cst.Expression (Parameters (Parameters'))
import Octizys.Effects.Parser.Backend
  ( ParserError
  , ParserState
  , prettyParserError
  )
import Octizys.Effects.Parser.Combinators (eof)
import Octizys.Effects.Parser.Effect (Parser)
import Octizys.Effects.Parser.Interpreter (runFullParser)
import Octizys.Effects.SymbolResolution.Effect (SymbolResolution)
import Octizys.Effects.SymbolResolution.Interpreter
  ( SymbolResolutionError
  , SymbolResolutionState
  , initialSymbolResolutionState
  , runSymbolResolutionFull
  )
import Octizys.Parser.Expression
  ( boolParser
  , functionParser
  , ifParser
  , intParser
  , letParser
  , parametersParser
  , variableParser
  )
import Octizys.Parser.TopItem (parseModule)
import Octizys.Pretty.Comment (prettyComment)
import Octizys.Pretty.Expression
  ( prettyExpression
  , prettyFunction
  , prettyParameters
  )
import Octizys.Pretty.TopItem (prettyModule)
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
        : Error SymbolResolutionError
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
          : Error SymbolResolutionError
          : '[]
      )
      a
  -> Text
  -> Either
      SymbolResolutionError
      (Either (ParserError OctizysParseError) a)
runParser p t = do
  out <-
    runPureEff $
      runErrorNoCallStack $
        runSymbolResolutionFull
          initialSymbolResolutionState
          (runFullParser t p)
  pure (fst out)


renderError :: Text -> ParserError OctizysParseError -> String
renderError source =
  render
    <<< prettyParserError pretty (Just ('t' :| "est")) source


render :: Doc ann -> String
render =
  Prettyprinter.Render.String.renderString
    <<< Prettyprinter.layoutPretty Prettyprinter.defaultLayoutOptions


stripSpacesAndNewlines :: String -> String
stripSpacesAndNewlines = filter (`notElem` [' ', '\n', '\r', '\t'])


shouldParse
  :: Eq a
  => Text
  -> (a -> Doc ann)
  -> Either (ParserError OctizysParseError) a
  -> String
  -> Expectation
shouldParse input _ (Left e) expected = do
  expectationFailure (renderError input e <> "\n" <> "Expected:" <> expected)
shouldParse _ toDoc (Right result) expected =
  let prettyResult = render (toDoc result)
   in if stripSpacesAndNewlines
        prettyResult
        == stripSpacesAndNewlines
          expected
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
  describe "comment parsers" $ do
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
  describe "expressions parser" $ do
    makePositiveTest
      "bool true"
      "true"
      (Just "true")
      boolParser
      (prettyExpression pretty pretty)
    makePositiveTest
      "bool false"
      "false"
      (Just "false")
      boolParser
      (prettyExpression pretty pretty)
    makePositiveTest
      "int zero"
      "0"
      Nothing
      intParser
      (prettyExpression pretty pretty)
    makePositiveTest
      "int no _ in int"
      "3487982"
      Nothing
      intParser
      (prettyExpression pretty pretty)
    makePositiveTest
      "int"
      "34_87_98___2"
      Nothing
      intParser
      (prettyExpression pretty pretty)
    makePositiveTest
      "variable"
      "abcde"
      (Just "ExpVarId[0]")
      variableParser
      (prettyExpression pretty pretty)
    makePositiveTest
      "function (lambdas)"
      "\\ a b c d -> c"
      -- TODO: the SymbolResolutionState is broken, it needs to help us
      -- catch this and have c the same identifier in both sides.
      ( Just
          "\\\n  ExpVarId[0]\n  ExpVarId[1]\n  ExpVarId[2]\n  ExpVarId[3] \n-> ExpVarId[2]"
      )
      functionParser
      (prettyFunction pretty pretty)
    makePositiveTest
      "if"
      "if 1 then 2 else 3"
      Nothing
      ifParser
      (prettyExpression pretty pretty)
    makePositiveTest
      "let single"
      "let a = 1; in 3"
      (Just "let ExpVarId[0] = 1; in 3")
      letParser
      (prettyExpression pretty pretty)
    makePositiveTest
      "let two vars"
      "let a = 1; b =2; in 3"
      (Just "let ExpVarId[0] = 1; ExpVarId[1] = 2; in 3")
      letParser
      (prettyExpression pretty pretty)
    makePositiveTest
      "let five vars"
      "let a = 1; b =2; c=3; d=4; e=5; in 6"
      ( Just
          "let ExpVarId[0] = 1; ExpVarId[1] = 2; ExpVarId[2] = 3;ExpVarId[3] = 4;ExpVarId[4] = 5;in 6"
      )
      letParser
      (prettyExpression pretty pretty)
    makePositiveTest
      "let capture avoid params"
      "let f: x , Int = x; g:x , Bool = x; in g 6"
      ( Just
          "let ExpVarId[0]: ExpVarId[1], Int = ExpVarId[1]; ExpVarId[2]:ExpVarId[3], Bool = ExpVarId[3]; in ExpVarId[2] 6"
      )
      letParser
      (prettyExpression pretty pretty)
    makePositiveTest
      "parameters 2"
      -- The `,` at the end is needed as we use it to know that we parsed a
      -- argument, if it is missing we fail!
      "x:Bool,y:Int,"
      (Just "ExpVarId[0]:Bool,ExpVarId[1]:Int")
      ((Parameters' <$>) <$> parametersParser)
      ( \(x :: Maybe Parameters) ->
          maybe
            ( pretty
                @Text
                "Failed to parse"
            )
            (prettyParameters pretty pretty)
            x
      )
    -- TODO: remove the last parens of this test.
    -- This means modify pretty to skip this paren
    -- at the end of arguments definition.
    -- This comma is not included int the input.
    makePositiveTest
      "parameters 3"
      "x:Bool,y:Int,z:(Bool->Int),"
      (Just "ExpVarId[0]:Bool,ExpVarId[1]:Int,ExpVarId[2]:Bool->Int")
      ((Parameters' <$>) <$> parametersParser)
      ( \(x :: Maybe Parameters) ->
          maybe
            ( pretty
                @Text
                "Failed to parse"
            )
            (prettyParameters pretty pretty)
            x
      )
    makePositiveTest
      "factorial example"
      "let fact = \\ n -> if lt n 2 then 1 else mul n (fact (minus n 1 )); in fact 5"
      ( Just
          "let ExpVarId[0] = \\ ExpVarId[1] -> if ExpVarId[2] ExpVarId[1] 2 then 1 else ExpVarId[3] ExpVarId[1] (ExpVarId[0] (ExpVarId[4] ExpVarId[1] 1 )); in ExpVarId[0] 5"
      )
      (letParser <* eof @OctizysParseError)
      (prettyExpression pretty pretty)
  describe "module" $ do
    makePositiveTest
      "single definition"
      "add_one: n:Int, m:Int, j:Int, Int = addition n 1;"
      ( Just
          "ExpVarId[0]: ExpVarId[1]: Int, ExpVarId[2]:Int, ExpVarId[3]:Int, Int = ExpVarId[4] ExpVarId[1] 1"
      )
      parseModule
      (prettyModule pretty pretty)


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
      case result of
        Left e -> expectationFailure ("SymbolResolution bug:" <> show e)
        Right r ->
          shouldParse input prettier r expected

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
