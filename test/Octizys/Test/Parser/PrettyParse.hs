{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Octizys.Test.Parser.PrettyParse (tests) where

import Control.Arrow ((<<<))
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
  , makeParseErrorReport
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
import Octizys.Parser.Common
  ( OctizysParseError
  , comment
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
import Octizys.Pretty.FormatContext (FormatContext, defaultFormatContext)
import Octizys.Pretty.Formatter (Formatter (format))
import Prettyprinter
  ( Doc
  , Pretty (pretty)
  , defaultLayoutOptions
  , layoutPretty
  )
import qualified Prettyprinter as Pretty
import qualified Prettyprinter.Render.Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)


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


renderError :: Text -> ParserError OctizysParseError -> Text
renderError source =
  render
    <<< format @() defaultFormatContext
    <<< makeParseErrorReport @() defaultFormatContext (Just ('t' :| "est")) source


render :: Doc ann -> Text
render =
  Prettyprinter.Render.Text.renderStrict
    <<< Prettyprinter.layoutPretty Prettyprinter.defaultLayoutOptions


stripSpacesAndNewlines :: Text -> Text
stripSpacesAndNewlines = Text.filter (not . (`elem` [' ', '\n', '\r', '\t']))


expectationFailure :: Text -> IO a
expectationFailure = assertFailure <<< Text.unpack


shouldParse
  :: forall a
   . Eq a
  => Formatter () (FormatContext ()) a
  => Text
  -> Either (ParserError OctizysParseError) a
  -> Text
  -> Assertion
shouldParse input (Left e) expected = do
  expectationFailure (renderError input e <> "\n" <> "Expected:" <> expected)
shouldParse _ (Right result) expected =
  let prettyResult = render (format @() defaultFormatContext result)
   in if stripSpacesAndNewlines
        prettyResult
        == stripSpacesAndNewlines
          expected
        then pure ()
        else
          expectationFailure
            ( render
                ( pretty @Text "Expected:"
                    <> pretty expected
                    <> Pretty.line
                    <> "Got:"
                    <> format @() defaultFormatContext result
                )
            )


{- | This module is on charge of testing:
"(render <<< pretty <<< parser)"
-}
tests :: TestTree
tests =
  testGroup
    ""
    [ testGroup
        "comment parsers"
        [ makePositiveTest
            "line comment"
            "-- hola mundo\n"
            (Just "-- hola mundo")
            (comment <* eof @OctizysParseError)
        , makePositiveTest
            "block comment"
            "{- hola mundo\n como estas?\n he?\n bye!-}"
            Nothing
            comment
        ]
    , -- TODO: test offsets!
      testGroup
        "expressions parser"
        [ makePositiveTest
            "bool True"
            "True"
            Nothing
            boolParser
        , makePositiveTest
            "bool False"
            "False"
            Nothing
            boolParser
        , makePositiveTest
            "int zero"
            "0"
            Nothing
            intParser
        , makePositiveTest
            "int no _ in int"
            "3487982"
            Nothing
            intParser
        , makePositiveTest
            "int"
            "34_87_98___2"
            Nothing
            intParser
        , makePositiveTest
            "variable"
            "abcde"
            (Just "_e0")
            variableParser
        , makePositiveTest
            "function (lambdas)"
            "\\ a b c d -> c"
            -- TODO: the SymbolResolutionState is broken, it needs to help us
            -- catch this and have c the same identifier in both sides.
            ( Just
                "\\\n  _e0\n  _e1\n  _e2\n  _e3 \n-> _e2"
            )
            functionParser
        , makePositiveTest
            "if"
            "if 1 then 2 else 3"
            Nothing
            ifParser
        , makePositiveTest
            "let single"
            "let a = 1; in 3"
            (Just "let _e0 = 1; in 3")
            letParser
        , makePositiveTest
            "let two vars"
            "let a = 1; b =2; in 3"
            (Just "let _e0 = 1; _e1 = 2; in 3")
            letParser
        , makePositiveTest
            "let five vars"
            "let a = 1; b =2; c=3; d=4; e=5; in 6"
            ( Just
                "let _e0 = 1; _e1 = 2; _e2 = 3;_e3 = 4;_e4 = 5;in 6"
            )
            letParser
        , makePositiveTest
            "let capture avoid params"
            "let f: x , Int = x; g:x , Bool = x; in g 6"
            ( Just
                "let _e1: _e0, Int = _e0; _e3:_e2, Bool = _e2; in _e3 6"
            )
            letParser
        , makePositiveTest
            "parameters 2"
            -- The `,` at the end is needed as we use it to know that we parsed a
            -- argument, if it is missing we fail!
            "x:Bool,y:Int,"
            (Just "_e0:Bool,_e1:Int")
            ((Parameters' <$>) <$> parametersParser)
        , -- TODO: remove the last parens of this test.
          -- This means modify pretty to skip this paren
          -- at the end of arguments definition.
          -- This comma is not included int the input.
          makePositiveTest
            "parameters 3"
            "x:Bool,y:Int,z:(Bool->Int),"
            (Just "_e0:Bool,_e1:Int,_e2:Bool->Int")
            ((Parameters' <$>) <$> parametersParser)
        , makePositiveTest
            "factorial example"
            "let fact = \\ n -> if lt n 2 then 1 else mul n (fact (minus n 1 )); in fact 5"
            ( Just
                "let _e0 = \\ _e1 -> if _e2 _e1 2 then 1 else _e3 _e1 (_e0 (_e4 _e1 1 )); in _e0 5"
            )
            (letParser <* eof @OctizysParseError)
        ]
    , testGroup
        "module"
        [ makePositiveTest
            "single definition"
            "add_one: n:Int, m:Int, j:Int, Int = addition n 1;"
            ( Just
                "_e3: _e0: Int, _e1:Int, _e2:Int, Int = _e4 _e0 1"
            )
            parseModule
        ]
    ]


--      (\(a,source)-> pretty a <> pretty source)

makePositiveTest
  :: Eq a
  => Formatter () (FormatContext ()) a
  => Text
  -> Text
  -> Maybe Text
  -> P a
  -> TestTree
makePositiveTest
  testName
  input
  maybeExpect
  parser =
    do
      testCase (Text.unpack testName) $ do
        let result = runParser parser input
        let expected :: Text =
              Data.Maybe.fromMaybe input maybeExpect
        case result of
          Left e -> expectationFailure ("SymbolResolution bug:" <> Text.pack (show e))
          Right r ->
            shouldParse input r expected

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
-- cleanText :: Text -> Text
-- cleanText = filter (`notElem` [' ', '\n', '\t', '\r'])
--
--
-- makePositiveTest
--  :: forall a
--   . Pretty a
--  => Parser a
--  -> Text
--  -> SpecWith ()
-- makePositiveTest p s =
--  it s $ do
--    let parsed = testParser p s
--    case parsed of
--      Right r -> cleanText (render r) `shouldBe` cleanText s
--      Left e -> (expectationFailure <<< errorBundlePretty) e
--
--
-- testPositiveExpression :: Text -> SpecWith ()
-- testPositiveExpression =
--  makePositiveTest (expressionParser <* eof)
--
--
-- testPositiveType :: Text -> SpecWith ()
-- testPositiveType =
--  makePositiveTest (typeParser <* eof)
--
--
-- testPositiveTopItem :: Text -> SpecWith ()
-- testPositiveTopItem =
--  makePositiveTest (topParser <* eof)
--
--
-- makePositiveFileTest
--  :: forall a
--   . Pretty a
--  => Parser [a]
--  -> Text
--  -> Text
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
-- testPositiveFile :: Text -> SpecWith ()
-- testPositiveFile path = do
--  content <- runIO $ readFile path
--  makePositiveFileTest (moduleParser <* eof) path content
--
--
-- fixtureFactorial :: Text
-- fixtureFactorial =
--  "let fact = \\ n -> if lt n 2 then 1 else mul n (fact (minus n 1 )); in fact 5"
