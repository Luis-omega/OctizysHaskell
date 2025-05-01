{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

{- | This module contains all the definitions needed to define the `Parser` effect.
it is unstable.
-}
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
  , prettyUserError
  , prettyParserError
  ) where

import Control.Arrow ((<<<))
import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Octizys.Cst.Span (Position, makeInitialPosition)
import qualified Octizys.Cst.Span as Span
import Prettyprinter (Doc, Pretty (pretty), (<+>))
import qualified Prettyprinter as Pretty


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


instance Pretty Expected where
  pretty expt =
    case expt of
      ExpectedRaw raw -> pretty raw
      ExpectedName name -> pretty name
      ExpectedEndOfInput -> pretty @String "end of input"


newtype Expectations = Expectations' {unExpectations :: Set Expected}
  deriving (Show, Eq, Ord)


instance Pretty Expectations where
  pretty es =
    let asList :: [Expected] = Set.toList $ unExpectations es
     in case asList of
          [] -> pretty @String ""
          (start : end) ->
            ( Pretty.align
                <<< Pretty.fillSep
            )
              ( pretty start
                  : ( (\x -> pretty @String "," <> pretty x) <$> end
                    )
              )


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


instance Pretty Unexpected where
  pretty expt =
    case expt of
      UnexpectedRaw raw -> pretty raw
      UnexpectedEndOfInput -> pretty @String "end of input"


data UserError e
  = SimpleError {message :: Text.Text}
  | CustomError {customError :: e}
  deriving (Show, Eq, Ord)


prettyUserError
  :: (e -> Doc ann)
  -> UserError e
  -> Doc ann
prettyUserError prettyErr err =
  case err of
    SimpleError {message = msg} -> pretty msg
    CustomError {customError = userE} -> prettyErr userE


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


-- TODO: add source of error
prettyParserError
  :: (e -> Doc ann)
  -> Maybe (NonEmpty Char)
  -> Text.Text
  -> ParserError e
  -> Doc ann
prettyParserError prettyError maybeName source err =
  let sourceName =
        maybe
          (pretty '>')
          (\n -> pretty n <> pretty @String ">")
          maybeName
      locationInfo pos =
        pretty @String "Error>"
          <> sourceName
          <> "line"
          <+> pretty (Span.line pos)
          <> ">column"
          <+> pretty (Span.column pos)
          <> pretty ':'
      prev =
        Text.takeWhileEnd ('\n' /=) $
          getPreviousChars source err.errorPosition.offset
      after =
        takeWithoutLineBreaks $
          getAfterChars source err.errorPosition.offset
      lenPrev = Text.length prev
      preText = Text.replicate lenPrev (Text.singleton ' ') <> Text.singleton '^'
   in case err of
        GeneratedError
          { errorPosition = pos
          , unexpected = unex
          , expected = expt
          } ->
            locationInfo pos
              <> Pretty.nest
                4
                ( Pretty.hardline
                    <> pretty (prev <> after)
                    <> Pretty.hardline
                    <> pretty preText
                    <> pretty @String "Unexpected"
                    <+> pretty unex
                    <> Pretty.hardline
                    <> pretty @String "Expected one of:"
                    <+> pretty expt
                )
        UserMadeError
          { errorPosition = pos
          , userErrors = userErrs
          } ->
            locationInfo pos
              <> Pretty.nest
                4
                ( Pretty.hardline
                    <> pretty (prev <> after)
                    <> Pretty.hardline
                    <> pretty preText
                    <> let asList = Set.toList userErrs
                        in (Pretty.align <<< Pretty.fillSep)
                            (prettyUserError prettyError <$> asList)
                )


instance Ord e => Semigroup (ParserError e) where
  (<>) e1 e2 =
    case compare (errorPosition e1) (errorPosition e2) of
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
  , remainStream :: !Text.Text
  , expected :: Expectations
  }
  deriving (Show, Eq, Ord)


makeInitialState :: Text.Text -> ParserState
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


{- | ===
=== Error Reporting
===
-}
getPreviousChars :: Text.Text -> Int -> Text.Text
getPreviousChars txt i = Text.take len $ Text.drop start txt
  where
    start = max 0 (i - 10)
    len = min 10 i


-- | Devuelve los 10 caracteres posteriores al índice dado.
getAfterChars :: Text.Text -> Int -> Text.Text
getAfterChars txt i = Text.take 10 $ Text.drop i txt


takeWithoutLineBreaks :: Text.Text -> Text.Text
takeWithoutLineBreaks t = Text.takeWhile ('\n' /=) t
