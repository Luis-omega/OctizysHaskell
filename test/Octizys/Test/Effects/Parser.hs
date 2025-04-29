{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Octizys.Test.Effects.Parser where

import Control.Arrow ((<<<))
import qualified Data.Bifunctor as Bifunctor
import Data.Either (isLeft, isRight)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Effectful (Eff, runPureEff, (:>))
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.State.Static.Local (State, runState)
import Octizys.Effects.Parser.Combinators hiding (text)
import qualified Octizys.Effects.Parser.Combinators as C
import Octizys.Effects.Parser.Effect
  ( Parser
  )
import Octizys.Effects.Parser.Backend(
   ParserError
  , ParserState
  , makeInitialState
                                     )
import Octizys.Effects.Parser.Interpreter (runFullParser)
import qualified Octizys.Effects.Parser.Interpreter as Parser
import Test.Hspec


runParser
  :: Eff
      ( Parser String
          : State ParserState
          : Error (ParserError String)
          : '[]
      )
      a
  -> Text
  -> Either String a
runParser p t = runPureEff $ do
  res <- runFullParser t p
  pure $ Bifunctor.first show res


text = C.text @String

tests :: SpecWith ()
tests = do
  describe "between" $ do
    it "parses correctly surrounded by open and close" $ do
      let open = text "("
          close = text ")"
          p = text "abc"
      runParser (between @String open close p) "(abc)" `shouldBe` Right "abc"

    it "fails if the open parser doesn't match" $ do
      let open = text "["
          close = text "]"
          p = text "abc"
      runParser (between @String open close p) "(abc)" `shouldSatisfy` isLeft

    it "fails if the close parser doesn't match" $ do
      let open = text "("
          close = text "]"
          p = text "abc"
      runParser (between @String open close p) "(abc)" `shouldSatisfy` isLeft

  describe "item" $ do
    it "parses a single character" $ do
      runParser (item @String) "a" `shouldBe` Right 'a'

    it "fails when the input is empty" $ do
      runParser (item @String) "" `shouldSatisfy` isLeft

  describe "satisfy" $ do
    it "parses a character that satisfies the predicate" $ do
      let p c = c == 'a'
      runParser (satisfy @String Nothing p) "a" `shouldBe` Right 'a'

    it "fails when the character doesn't satisfy the predicate" $ do
      let p c = c == 'a'
      runParser (satisfy @String Nothing p) "b" `shouldSatisfy` isLeft

  describe "lookupNext" $ do
    it "returns the next item" $ do
      runParser (lookupNext @String) "abc" `shouldBe` Right (Just 'a')

    it "success if input is empty" $ do
      runParser (lookupNext @String) "" `shouldBe` Right Nothing

  describe "takeWhileP" $ do
    it "parses characters while the predicate is true" $ do
      let p c = c == 'a'
      runParser (takeWhileP @String p) "aaa" `shouldBe` Right "aaa"

    it "stops when the predicate is false" $ do
      let p c = c == 'a'
      runParser (takeWhileP @String p) "aab" `shouldBe` Right "aa"

    it "fails if no characters satisfy the predicate" $ do
      let p c = c == 'a'
      runParser (takeWhileP @String p) "b" `shouldBe` Right ""

  describe "takeWhile1P" $ do
    it "parses at least one character while the predicate is true" $ do
      let p c = c == 'a'
      runParser (takeWhile1P @String (Just "a") p) "aaa" `shouldBe` Right "aaa"

    it "fails if no characters satisfy the predicate" $ do
      let p c = c == 'a'
      runParser (takeWhile1P @String Nothing p) "b" `shouldSatisfy` isLeft

    it "fails if input is empty" $ do
      let p c = c == 'a'
      runParser (takeWhile1P @String Nothing p) "" `shouldSatisfy` isLeft

  describe "try" $ do
    it "backtracks correctly on failure" $ do
      let p = alternative @String (try @String (text "abc")) (text "abdef")
      runParser p "abdef" `shouldBe` Right "abdef"

    it "fails if neither parser succeeds" $ do
      let p = alternative @String (try @String (text "abc")) (text "def")
      runParser p "xyz" `shouldSatisfy` isLeft

  describe "alternative" $ do
    it "parses the first parser" $ do
      let p = alternative @String (char @String 'a') (char @String 'b')
      runParser p "a" `shouldBe` Right 'a'

    it "parses the second parser if the first fails" $ do
      let p = alternative @String (char @String 'a') (char @String 'b')
      runParser p "b" `shouldBe` Right 'b'

    it "fails if both parsers fail" $ do
      let p =
            label @String
              ('n' :| "o soy yo, eres tu")
              (alternative @String (char @String 'a') (char @String 'b'))
      runParser p "c" `shouldSatisfy` isRight

