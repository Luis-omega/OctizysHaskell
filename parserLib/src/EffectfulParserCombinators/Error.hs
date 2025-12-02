module EffectfulParserCombinators.Error where

import EffectfulParserCombinators.Expectation (Expectations)
import EffectfulParserCombinators.Span (Position)
import EffectfulParserCombinators.Unexpected
  ( Unexpected (UnexpectedEndOfInput, UnexpectedRaw)
  )

import Control.Arrow ((<<<))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified EffectfulParserCombinators.Span as Span
import Prettyprinter (Doc, Pretty (pretty), (<+>))
import qualified Prettyprinter as Pretty


data UserError e
  = SimpleError {message :: Text.Text}
  | CustomError {customError :: e}
  deriving (Show, Eq, Ord)


-- instance
--   Formatter ann (FormatContext ann) e
--   => Formatter ann (FormatContext ann) (UserError e)
--   where
--   format ctx usError = prettyUserError (format ctx) usError

instance (Show e, Pretty e) => Pretty (UserError e) where
  pretty (SimpleError msg) = pretty msg
  pretty (CustomError e) = pretty e


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


humanReadableError
  :: forall ann e
   . Pretty e
  => Show e
  => Maybe (NonEmpty Char)
  -> Text.Text
  -> ParserError e
  -> Doc ann
humanReadableError maybeName src err =
  let
    sourceName :: String
    sourceName =
      maybe "" ((<> ">") <<< NonEmpty.toList) maybeName
    locationInfo :: Position -> String
    locationInfo pos =
      "ParserError>"
        <> sourceName
        <> "line "
        <> show (Span.line pos)
        <> ">column "
        <> show (Span.column pos)
        <> ":"
    prev =
      Text.takeWhileEnd ('\n' /=) $
        getPreviousChars src err.errorPosition.offset
    after =
      takeWithoutLineBreaks $
        getAfterChars src err.errorPosition.offset
    lenPrev = Text.length prev
    preText = Text.replicate lenPrev (Text.singleton ' ') <> Text.singleton '^'
    description = pretty @Text "Unrecognized input while parsing:"
   in
    case err of
      GeneratedError
        { errorPosition = pos
        , unexpected = unex
        , expected = expt
        } ->
          let
            locationText = pretty (locationInfo pos)
            source =
              pretty (prev <> after)
                <> Pretty.hardline
                <> pretty preText
                <> pretty @String "Unexpected"
                <+> pretty unex
                <> Pretty.hardline
                <> pretty @String "Expected one of:"
                <+> pretty expt
           in
            locationText
              <> Pretty.line
              <> description
              <> Pretty.line
              <> source
      UserMadeError
        { errorPosition = pos
        , userErrors = userErrs
        } ->
          let
            locationText = pretty (locationInfo pos)
            source =
              pretty (prev <> after)
                <> Pretty.hardline
                <> pretty preText
                <> let asList = Set.toList userErrs
                    in (Pretty.align <<< Pretty.fillSep)
                        (pretty <$> asList)
           in
            locationText
              <> Pretty.line
              <> description
              <> Pretty.line
              <> source


-- makeParseErrorReport
--   :: Formatter ann (FormatContext ann) e
--   => FormatContext ann
--   -> Maybe (NonEmpty Char)
--   -> Text.Text
--   -> ParserError e
--   -> Report ann
-- makeParseErrorReport ctx maybeName src err =
--   let
--     sourceName :: String
--     sourceName =
--       maybe "" ((<> ">") <<< NonEmpty.toList) maybeName
--     locationInfo :: Position -> String
--     locationInfo pos =
--       "ParserError>"
--         <> sourceName
--         <> "line "
--         <> show (Span.line pos)
--         <> ">column "
--         <> show (Span.column pos)
--         <> ":"
--     prev =
--       Text.takeWhileEnd ('\n' /=) $
--         getPreviousChars src err . errorPosition . offset
--     after =
--       takeWithoutLineBreaks $
--         getAfterChars src err . errorPosition . offset
--     lenPrev = Text.length prev
--     preText = Text.replicate lenPrev (Text.singleton ' ') <> Text.singleton '^'
--    in
--     case err of
--       GeneratedError
--         { errorPosition = pos
--         , unexpected = unex
--         , expected = expt
--         } ->
--           Report'
--             { reportKind = ReportError
--             , shortDescription = pack $ locationInfo pos
--             , descriptions =
--                 [ LongDescription'
--                     { -- TODO: FIXME : this usseles message is here for only
--                       -- one reason, the report formatting adds a nest to
--                       -- source and without a preDescription we don't have a
--                       -- line break previously, this de-sync the '^' in the
--                       -- error report from their source...
--                       preDescription = Just "Unrecognized input while parsing:"
--                     , source =
--                         Just
--                           ( pretty (prev <> after)
--                               <> Pretty.hardline
--                               <> pretty preText
--                               <> pretty @String "Unexpected"
--                                 <+> pretty unex
--                               <> Pretty.hardline
--                               <> pretty @String "Expected one of:"
--                                 <+> pretty expt
--                           )
--                     , afterDescription = Nothing
--                     }
--                 ]
--             }
--       UserMadeError
--         { errorPosition = pos
--         , userErrors = userErrs
--         } ->
--           Report'
--             { reportKind = ReportError
--             , shortDescription = pack $ locationInfo pos
--             , descriptions =
--                 [ LongDescription'
--                     { -- TODO: FIXME: Read the previous one.
--                       preDescription = Just "Unrecognized input while parsing:"
--                     , source =
--                         Just
--                           ( pretty (prev <> after)
--                               <> Pretty.hardline
--                               <> pretty preText
--                               <> let asList = Set.toList userErrs
--                                   in (Pretty.align <<< Pretty.fillSep)
--                                       (format ctx <$> asList)
--                           )
--                     , afterDescription = Nothing
--                     }
--                 ]
--             }

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


{- | ===
=== Error Reporting Functions
===
-}

-- | Returns the 10 characters before the given position.
getPreviousChars :: Text.Text -> Int -> Text.Text
getPreviousChars txt i = Text.take len $ Text.drop start txt
  where
    start = max 0 (i - 10)
    len = min 10 i


-- | Returns the 10 characters after the given position.
getAfterChars :: Text.Text -> Int -> Text.Text
getAfterChars txt i = Text.take 10 $ Text.drop i txt


takeWithoutLineBreaks :: Text.Text -> Text.Text
takeWithoutLineBreaks = Text.takeWhile ('\n' /=)
