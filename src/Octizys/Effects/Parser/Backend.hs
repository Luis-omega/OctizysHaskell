-- | This module contains all the definitions needed to define the `Parser` effect.
-- it is unstable.

{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
module Octizys.Effects.Parser.Backend
  ( Expected (ExpectedRaw, ExpectedName, ExpectedEndOfInput)
  , Unexpected (UnexpectedRaw, UnexpectedEndOfInput)
  , ParserError
    ( GeneratedError
    , UserMadeError
    , errorPosition
    , userErrors
    , unexpected
    , expected
    )
  , UserError (SimpleError, CustomError)
  , Expectations (Expectations')
  , ParserState
    ( ParserState'
    , position
    , remainStream
    , expected
    )
  , insertExpectation
  , makeInitialState
  , addExpectation
  , mergeExpectations
  , emptyExpectations
  , singletonExpectations
  ) where

import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text
import Octizys.Cst.Span (Position, makeInitialPosition)
import Control.Arrow ((<<<))


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



newtype Expectations
  = Expectations' (Set Expected)
  deriving (Show, Eq, Ord)


insertExpectation :: Expected -> Expectations -> Expectations
insertExpectation e (Expectations' es) = Expectations' (Set.insert e es)


mergeExpectations :: Expectations -> Expectations -> Expectations
mergeExpectations (Expectations' es') (Expectations' es) =
  Expectations' (Set.union es' es)


emptyExpectations :: Expectations
emptyExpectations = Expectations' Set.empty

singletonExpectations :: Expected -> Expectations
singletonExpectations = Expectations' <<< Set.singleton


instance Semigroup Expectations where
  (<>) = mergeExpectations


instance Monoid Expectations where
  mempty = emptyExpectations


-- | Represent a unexpected element in the stream.
data Unexpected
  = -- | The original chain of characters.
    UnexpectedRaw (NonEmpty Char)
  | -- | Reached end of input
    UnexpectedEndOfInput
  deriving (Show, Eq, Ord)


data UserError e
  = SimpleError {message :: Text}
  | CustomError {customError :: e}
  deriving (Show, Eq, Ord)


data ParserError e
  = GeneratedError
      { errorPosition :: Position
      , unexpected :: Unexpected
      , expected :: Expectations
      }
  | UserMadeError
      { errorPosition :: Position
      , userErrors :: Set (UserError e)
      }
  deriving (Show, Eq, Ord)


instance Ord e => Semigroup (ParserError e) where
  (<>) e1 e2 =
    case compare (userErrors e1) (userErrors e2) of
      LT -> e2
      GT -> e1
      _ ->
        case (e1, e2) of
          ( GeneratedError
              { errorPosition = p1
              , unexpected = u1
              , expected = exp1
              }
            , GeneratedError {unexpected = u2, expected = exp2}
            ) ->
              GeneratedError
                { errorPosition = p1
                , unexpected = mergeU u1 u2
                , expected = exp1 <> exp2
                }
          (GeneratedError {}, x) -> x
          (x, GeneratedError {}) -> x
          ( UserMadeError
              { errorPosition = p1
              , userErrors = userError1
              }
            , UserMadeError {userErrors = userError2}
            ) ->
              UserMadeError
                { errorPosition = p1
                , userErrors = Set.union userError1 userError2
                }
    where
      -- We need to choose one...
      mergeU u1 u2 =
        case (u1, u2) of
          (UnexpectedRaw l1, UnexpectedRaw l2) ->
            if NonEmpty.length l1 < NonEmpty.length l2
              then UnexpectedRaw l2
              else UnexpectedRaw l1
          (UnexpectedEndOfInput, _) -> UnexpectedEndOfInput
          (_, UnexpectedEndOfInput) -> UnexpectedEndOfInput


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


addExpectation :: Expected -> ParserState -> ParserState
addExpectation expt s =
  let newSet = coerce $ Set.insert expt (coerce s.expected)
   in s {expected = newSet}
