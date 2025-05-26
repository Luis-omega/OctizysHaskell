{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Octizys.Test.Effects.Parser where

import Control.Arrow ((<<<))
import qualified Data.Bifunctor as Bifunctor
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Effectful (Eff, runPureEff)
import Effectful.Error.Static (Error)
import Effectful.State.Static.Local (State)
import Octizys.Effects.Parser.Backend
  ( ParserError
  , ParserState
  , makeParseErrorReport
  )
import Octizys.Effects.Parser.Combinators hiding (text)
import qualified Octizys.Effects.Parser.Combinators as C
import Octizys.Effects.Parser.Effect
  ( Parser
  )
import Octizys.Effects.Parser.Interpreter (runFullParser)
import Octizys.Pretty.FormatContext (defaultFormatContext)
import Octizys.Pretty.Formatter (Formatter (format))
import qualified Prettyprinter
import qualified Prettyprinter.Render.Text
import Test.Tasty
import Test.Tasty.HUnit


render :: Text -> ParserError Text -> Text
render source =
  Prettyprinter.Render.Text.renderStrict
    <<< Prettyprinter.layoutPretty Prettyprinter.defaultLayoutOptions
    <<< format @() defaultFormatContext
    <<< makeParseErrorReport @() defaultFormatContext (Just ('t' :| "est")) source


runParser
  :: Eff
      ( Parser Text
          : State ParserState
          : Error (ParserError Text)
          : '[]
      )
      a
  -> Text
  -> Either Text a
runParser p t = runPureEff $ do
  res <- runFullParser t p
  pure $ Bifunctor.first (render t) res


text
  :: Text
  -> Eff [Parser Text, State ParserState, Error (ParserError Text)] Text
text = C.text @Text


shouldSatisfy :: a -> (a -> Bool) -> Assertion
shouldSatisfy value predicate =
  predicate value @? ""


tests :: TestTree
tests =
  testGroup
    "hi"
    [ testGroup
        "between"
        [ testCase "parses correctly surrounded by open and close" $ do
            let open = text "("
                close = text ")"
                p = text "abc"
            runParser (between @Text open close p) "(abc)" @?= Right "abc"
        , testCase "fails if the open parser doesn't match" $ do
            let open = text "["
                close = text "]"
                p = text "abc"
            runParser (between @Text open close p) "(abc)" `shouldSatisfy` isLeft
        , testCase "fails if the close parser doesn't match" $ do
            let open = text "("
                close = text "]"
                p = text "abc"
            runParser (between @Text open close p) "(abc)" `shouldSatisfy` isLeft
        ]
    , testGroup
        "item"
        [ testCase "parses a single character" $ do
            runParser (item @Text) "a" @?= Right 'a'
        , testCase "fails when the input is empty" $ do
            runParser (item @Text) "" `shouldSatisfy` isLeft
        ]
    , testGroup
        "satisfy"
        [ testCase "parses a character that satisfies the predicate" $
            let p c = c == 'a'
             in runParser (satisfy @Text Nothing p) "a"
                  @?= Right
                    'a'
        , testCase
            "fails when the character doesn't satisfy the predicate"
            $ let p c = c == 'a'
               in runParser (satisfy @Text Nothing p) "b" `shouldSatisfy` isLeft
        ]
    , testGroup
        "lookupNext"
        [ testCase "returns the next item" $ do
            runParser (lookupNext @Text) "abc" @?= Right (Just 'a')
        , testCase "success if input is empty" $ do
            runParser (lookupNext @Text) "" @?= Right Nothing
        ]
    , testGroup
        "takeWhileP"
        [ testCase "parses characters while the predicate is true" $ do
            let p c = c == 'a'
            runParser (takeWhileP @Text p) "aaa" @?= Right "aaa"
        , testCase "stops when the predicate is false" $ do
            let p c = c == 'a'
            runParser (takeWhileP @Text p) "aab" @?= Right "aa"
        , testCase "fails if no characters satisfy the predicate" $ do
            let p c = c == 'a'
            runParser (takeWhileP @Text p) "b" @?= Right ""
        ]
    , testGroup
        "takeWhile1P"
        [ testCase "parses at least one character while the predicate is true" $ do
            let p c = c == 'a'
            runParser (takeWhile1P @Text (Just "a") p) "aaa" @?= Right "aaa"
        , testCase "fails if no characters satisfy the predicate" $ do
            let p c = c == 'a'
            runParser (takeWhile1P @Text Nothing p) "b" `shouldSatisfy` isLeft
        , testCase "fails if input is empty" $ do
            let p c = c == 'a'
            runParser (takeWhile1P @Text Nothing p) "" `shouldSatisfy` isLeft
        ]
    , testGroup
        "try"
        [ testCase "backtracks correctly on failure" $ do
            let p = alternative @Text (try @Text (text "abc")) (text "abdef")
            runParser p "abdef" @?= Right "abdef"
        , testCase "fails if neither parser succeeds" $ do
            let p = alternative @Text (try @Text (text "abc")) (text "def")
            runParser p "xyz" `shouldSatisfy` isLeft
        ]
    , testGroup
        "alternative"
        [ testCase "parses the first parser" $ do
            let p = alternative @Text (char @Text 'a') (char @Text 'b')
            runParser p "a" @?= Right 'a'
        , testCase "parses the second parser if the first fails" $ do
            let p = alternative @Text (char @Text 'a') (char @Text 'b')
            runParser p "b" @?= Right 'b'
        , testCase "fails if both parsers fail" $ do
            let p =
                  alternative @Text (char @Text 'a') (char @Text 'b')
            runParser p "c" `shouldSatisfy` isLeft
        ]
    ]
