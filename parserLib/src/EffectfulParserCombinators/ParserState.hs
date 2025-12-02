module EffectfulParserCombinators.ParserState where

import EffectfulParserCombinators.Expectation
  ( Expectations
  , Expected
  )
import qualified EffectfulParserCombinators.Expectation as Expectation
import EffectfulParserCombinators.Span (Position, makeInitialPosition)

import qualified Data.Text as Text


data ParserState = ParserState'
  { position :: !Position
  , remainStream :: !Text.Text
  , expected :: Expectations
  }
  deriving (Show, Eq, Ord)


makeInitialState :: Text.Text -> ParserState
makeInitialState t =
  ParserState'
    { position = makeInitialPosition
    , remainStream = t
    , expected = Expectation.empty
    }


addExpectation :: Expected -> ParserState -> ParserState
addExpectation expt s =
  let
    expectations = s.expected
    newSet = Expectation.insert expt expectations
   in
    s {expected = newSet}
