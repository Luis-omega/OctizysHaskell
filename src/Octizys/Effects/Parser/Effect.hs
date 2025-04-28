{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module defines a `Parser` effect with basic parsing capabilities.
module Octizys.Effects.Parser.Effect
  ( Position (Position', line, column, offset)
  , Expected (ExpectedRaw, ExpectedName, ExpectedEndOfInput)
  , Unexpected (UnexpectedRaw, UnexpectedName, UnexpectedEndOfInput)
  , ParserError
    ( GeneratedErrror
    , SimpleError
    , CustomError
    , sourcePosition
    , unexpected
    , expected
    , message
    , userError
    )
  , Expectations (Expectations')
  , ParserState
    ( ParserState'
    , position
    , remainStream
    , expected
    )
  , Parser (ThrowParseError, CatchParseError, GetParseState, PutParseState)
  , throwParseError
  , putParseState
  , getParseState
  , catchParseError
  , makeInitialState
  , makeInitialPosition
  , mergeExpectations
  , insertExpectation
  ) where

import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text
import Effectful (Effect)
import Effectful.TH (makeEffect)


data Position = Position'
  { line :: Int
  -- ^ The amount of "\n" seen on stream
  , column :: Int
  -- ^ The amount of `Text` `Char`.
  , offset :: Int
  -- ^ The position on `Text` `Char` (ie unicode)
  }
  -- TODO: Use bytes

  deriving (Show, Eq, Ord)


makeInitialPosition :: Position
makeInitialPosition = Position' {line = 0, column = 0, offset = 0}


{- | Inspired by megaparsec, it represents expectation
of some input.
-}
data Expected
  = -- | A non empty string that was expected to be seen.
    ExpectedRaw (NonEmpty Char)
  | -- | A custom name for what we expected to see.
    ExpectedName (NonEmpty Char)
  | -- | Expected end of input.
    ExpectedEndOfInput
  deriving (Show, Eq, Ord)


-- | Represent a unexpected element in the stream.
data Unexpected
  = -- | The original chain of charactes.
    UnexpectedRaw (NonEmpty Char)
  | -- TODO: do we need this one?

    -- | A custom name for the unexpeted thing.
    UnexpectedName (NonEmpty Char)
  | -- | Reached end of input
    UnexpectedEndOfInput
  deriving (Show, Eq, Ord)


newtype Expectations
  = Expectations' (Set Expected)
  deriving (Show, Eq, Ord)


insertExpectation :: Expected -> Expectations -> Expectations
insertExpectation e (Expectations' es) = Expectations' (Set.insert e es)


mergeExpectations :: Expectations -> Expectations -> Expectations
mergeExpectations (Expectations' es') (Expectations' es) = Expectations' (Set.union es' es)


data ParserError e
  = GeneratedErrror
      { sourcePosition :: Position
      , unexpected :: Unexpected
      , expected :: Expectations
      }
  | SimpleError {message :: Text}
  | CustomError {userError :: e}
  deriving (Show, Eq, Ord)


data ParserState = ParserState'
  { position :: !Position
  , remainStream :: !Text
  , expected :: Expectations
  }
  deriving (Show, Eq, Ord)


makeInitialState :: Text -> ParserState
makeInitialState t =
  ParserState'
    { position = makeInitialPosition
    , remainStream = t
    , expected = coerce Set.empty
    }


-- | Effect for generating fresh integers with phantom types
data Parser e :: Effect where
  ThrowParseError :: ParserError e -> Parser e m a
  CatchParseError
    :: m a
    -> (ParserError e -> m a)
    -> Parser e m a
  GetParseState :: Parser e m ParserState
  PutParseState :: ParserState -> Parser e m ()


$(makeEffect ''Parser)
