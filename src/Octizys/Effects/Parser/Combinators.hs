{-# LANGUAGE UndecidableInstances #-}
{-# HLINT ignore "Use $>" #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Octizys.Effects.Parser.Combinators
  ( errorMessage
  , unexpectedEof
  , unexpectedRaw
  , errorCustom
  , emptyExpectations
  , item
  , text
  , satisfy
  , char
  , lookupNext
  , takeWhileP
  , takeWhile1P
  , try
  , alternative
  , optional
  , many
  , some
  , sepBy
  , sepBy1
  , between
  , chainl1
  , chainr1
  , manyTill
  , sepEndBy
  , (<|>)
  , fail
  , label
  , (<?>)
  , getPosition
  , eof
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Effectful (Eff, (:>))
import Octizys.Effects.Parser.Backend
  ( Expected (ExpectedEndOfInput, ExpectedName, ExpectedRaw)
  , ParserError
    ( GeneratedError
    , UserMadeError
    , errorPosition
    , expected
    , unexpected
    , userErrors
    )
  , ParserState (expected, position, remainStream)
  , Unexpected (UnexpectedEndOfInput, UnexpectedRaw)
  , UserError (CustomError, SimpleError)
  , addExpectation
  , emptyExpectations
  , singletonExpectations
  )
import Octizys.Effects.Parser.Effect
  ( Parser
  , catchParseError
  , getParseState
  , putParseState
  , throwParseError
  )

import Octizys.Cst.Span (Position (Position', column, line, offset))

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import Prelude hiding (exp, fail)


-- | == Auxiliary functions

{- | Apply a function to the current state and returns the
result.
-}
gets
  :: Parser e :> es
  => (ParserState -> b)
  -> Eff es b
gets f =
  f <$> getParseState


getPosition
  :: Parser e :> es
  => Eff es Position
getPosition = gets position


-- | === Errors

-- | Throw a parser error with the given message and the current position.
errorMessage
  :: Parser e :> es
  => Text
  -> Eff es a
errorMessage msg = do
  pos <- getPosition
  throwParseError $
    UserMadeError
      { errorPosition = pos
      , userErrors = Set.singleton (SimpleError msg)
      }


-- | Throw a error with the current position and the given custom error.
errorCustom
  :: Parser e :> es
  => e
  -> Eff es a
errorCustom err = do
  pos <- getPosition
  throwParseError $
    UserMadeError
      { errorPosition = pos
      , userErrors = Set.singleton (CustomError err)
      }


{- | Build a `GeneratedError` using the current parsing state and
    the given `Unexpected`.
-}
makeGeneratedError
  :: Parser e :> es
  => Unexpected
  -> Eff es (ParserError e)
makeGeneratedError unexpectedItem = do
  s <- getParseState
  pure
    GeneratedError
      { errorPosition = s.position
      , unexpected = unexpectedItem
      , expected = s.expected
      }


{- | Throws a end of input error with the current state as
information.
-}
unexpectedEof :: Parser e :> es => Eff es a
unexpectedEof = do
  err <- makeGeneratedError UnexpectedEndOfInput
  throwParseError err


{- | Throws a unexpected error with the current state as
information and the source information provided.
-}
unexpectedRaw
  :: Parser e :> es
  => NonEmpty Char
  -> Eff es a
unexpectedRaw originalText = do
  -- TODO: handle expected set
  err <- makeGeneratedError (UnexpectedRaw originalText)
  throwParseError err


-- | === State Updates

-- | Changes the `ParserState` using the given function.
modifyParserState
  :: Parser e :> es
  => (ParserState -> ParserState)
  -> Eff es ()
modifyParserState f = do
  s <- getParseState
  putParseState (f s)


-- | Updates the `Position` based on a character.
charUpdatePosition :: Position -> Char -> Position
charUpdatePosition s c =
  if c == '\n'
    then Position' {line = s.line + 1, column = 0, offset = s.offset + 1}
    else Position' {line = s.line, column = s.column + 1, offset = s.offset + 1}


{- | Updates the `ParserState`, it:
  - Updates the position based on the given Char
  - Sets the remain stream to the given stream
-}
charUpdateState :: Char -> Text -> ParserState -> ParserState
charUpdateState c newStream s =
  let newPos = charUpdatePosition s.position c
   in s {position = newPos, remainStream = newStream}


{- | Update the `ParserState` assuming it consumed the
given `Text` and sets the remain of the stream.
-}
textUpdateState
  :: Text
  -- ^ Parsed stream
  -> Text
  -- ^ Remain stream
  -> ParserState
  -- ^ State to update
  -> ParserState
textUpdateState parsed remain s =
  let newPosition = Text.foldl' charUpdatePosition s.position parsed
   in s {position = newPosition, remainStream = remain}


{- | Allow us to see the next character
and the remain text.
-}
lookupNextChar :: ParserState -> Maybe (Char, Text)
lookupNextChar s = Text.uncons s.remainStream


-- | == Basic combinators
fail
  :: Parser e :> es
  => Eff es Char
fail = errorMessage "fail"


{- | Gets the next char of the input, it throws a end of
input error if it doesn't exists.
-}
item
  :: Parser e :> es
  => Eff es Char
item = do
  s <- getParseState
  case Text.uncons s.remainStream of
    Just (current, remain) -> do
      modifyParserState $
        charUpdateState current remain
      pure current
    Nothing -> unexpectedEof


eof
  :: Parser e :> es
  => Eff es ()
eof = do
  maybeChar <- lookupNext
  case maybeChar of
    Nothing -> pure ()
    Just x -> do
      modifyParserState $ addExpectation ExpectedEndOfInput
      unexpectedRaw (x :| [])


text
  :: Parser e :> es
  => Text
  -> Eff es Text
text s =
  let l = Text.length s
   in case Text.unpack s of
        (sPrefix : sSuffix) ->
          do
            st <- getParseState
            let (prefix, more) = Text.splitAt l st.remainStream
             in if s == prefix
                  then modifyParserState (textUpdateState s more) >> pure s
                  else do
                    modifyParserState $
                      addExpectation
                        ( ExpectedRaw
                            (sPrefix :| sSuffix)
                        )
                    case Text.unpack prefix of
                      [] -> unexpectedEof
                      (pPrefix : pSuffix) -> unexpectedRaw (pPrefix :| pSuffix)
        _ -> pure ""


{- | Consumes a element from the input and returns it if it
 match the given predicate or throws a unexpected error.
-}
satisfy
  :: Parser e
    :> es
  => Maybe String
  -- ^ A name for a character matched by the predicate.
  -> (Char -> Bool)
  -- \^ Predicate
  -> Eff es Char
satisfy maybeName predicate = do
  maybeC <- lookupNext
  case maybeC of
    Just c ->
      if predicate c
        then item
        else case maybeName of
          Just (x : y) -> do
            modifyParserState (addExpectation (ExpectedName (x :| y)))
            unexpectedRaw (NonEmpty.singleton c)
          _ -> do
            unexpectedRaw (NonEmpty.singleton c)
    Nothing -> unexpectedEof


char
  :: Parser e :> es
  => Char
  -> Eff es Char
char c = satisfy (Just [c]) (c ==)


{- | Allow us to check the next element on the stream without
consuming input or returns Nothing if we reached the eof.
This function doesn't throw.
-}
lookupNext
  :: Parser e :> es
  => Eff es (Maybe Char)
lookupNext = (fst <$>) <$> gets lookupNextChar


{- | Takes elements from the input stream as long as
it matches the predicate. It never fails even
when no input is matched.
-}
takeWhileP
  :: Parser e :> es
  => (Char -> Bool)
  -- \^ Predicate
  -> Eff es Text
takeWhileP predicate = do
  stream <- gets remainStream
  let match = Text.takeWhile predicate stream
  if Text.null match
    then pure match
    else
      let newStream = Text.drop (Text.length match) stream
       in do
            modifyParserState (textUpdateState match newStream)
            pure match


{- | Takes elements from the input stream as long as
it matches the predicate. It parses at least one `Char`
otherwise it throws a error.
-}
takeWhile1P
  :: Parser e :> es
  => Maybe String
  -- ^ A name for the `Char`s matched by the predicate.
  -> (Char -> Bool)
  -- \^ Predicate
  -> Eff es Text
takeWhile1P maybeName predicate = do
  stream <- gets remainStream
  let match = Text.takeWhile predicate stream
  if Text.null match
    then case maybeName of
      Just (x : y) -> do
        modifyParserState (addExpectation (ExpectedName (x :| y)))
        unexpectedEof
      _ ->
        case Text.uncons stream of
          Just (c, _) -> do
            unexpectedRaw (c :| [])
          Nothing -> unexpectedEof
    else
      let newStream = Text.drop (Text.length match) stream
       in do
            modifyParserState (textUpdateState match newStream)
            pure match


-- | Run a parser, and if it fails, restores the previous state.
try
  :: Parser e :> es
  => Eff es a
  -> Eff es a
try p = do
  state <- getParseState
  p `catchParseError` \e ->
    putParseState state >> throwParseError e


{- | Runs the first parser, if it fails without consuming input, runs the second parser.
If the first parser consumes input and fails, the whole alternative fails.
-}
alternative
  :: Parser e :> es
  => Ord e
  => Eff es a
  -> Eff es a
  -> Eff es a
alternative p1 p2 = do
  originalState <- getParseState
  p1 `catchParseError` \err1 -> do
    afterP1State <- getParseState
    if position originalState /= position afterP1State
      then throwParseError err1
      else do
        p2 `catchParseError` \err2 -> do
          throwParseError (err1 <> err2)


-- p2 `catchParseError` (\ e ->
--   modifyParserState
--     (\s -> s {expected = mergeExpectations state.expected s.expected})
--            )

-- | Parses zero or one occurrence of p, returning Maybe.
optional
  :: Ord e
  => Parser e :> es
  => Eff es a
  -> Eff es (Maybe a)
optional p = try (Just <$> p) `alternative` pure Nothing


-- | Parses zero or more occurrences of p.
many
  :: Ord e
  => Parser e :> es
  => Eff es a
  -> Eff es [a]
many p = do
  ini <- optional p
  case ini of
    Nothing -> pure []
    Just y -> do
      res <- many p
      pure (y : res)


-- | Parses one or more occurrences of p.
some
  :: Ord e
  => Parser e :> es
  => Eff es a
  -> Eff es (NonEmpty a)
some p = do
  x <- p
  xs <- many p
  pure (x :| xs)


-- | Parses zero or more occurrences of p separated by sep.
sepBy
  :: Ord e
  => Parser e :> es
  => Eff es a
  -> Eff es sep
  -> Eff es [a]
sepBy p sep = sepBy1 p sep `alternative` pure []


-- | Parses one or more occurrences of p separated by sep.
sepBy1
  :: Ord e
  => Parser e :> es
  => Eff es a
  -> Eff es sep
  -> Eff es [a]
sepBy1 p sep = do
  x <- p
  xs <- many (sep *> p)
  pure (x : xs)


-- | Parses p, surrounded by open and close parsers.
between
  :: Parser e :> es
  => Eff es open
  -> Eff es close
  -> Eff es a
  -> Eff es a
between open close p = do
  _ <- open
  x <- p
  _ <- close
  pure x


-- | Parses left-associative binary operations.
chainl1
  :: Ord e
  => Parser e :> es
  => Eff es a
  -> Eff es (a -> a -> a)
  -> Eff es a
chainl1 p op = do
  x <- p
  rest x
  where
    rest x =
      ( do
          f <- op
          y <- p
          rest (f x y)
      )
        `alternative` pure x


-- | Parses right-associative binary operations.
chainr1
  :: Ord e
  => Parser e :> es
  => Eff es a
  -> Eff es (a -> a -> a)
  -> Eff es a
chainr1 p op = do
  x <- p
  ( do
      f <- op
      y <- chainr1 p op
      pure (f x y)
    )
    `alternative` pure x


-- | Parses many p until end parser succeeds.
manyTill
  :: Ord e
  => Parser e :> es
  => Eff es a
  -> Eff es end
  -> Eff es [a]
manyTill p end =
  (end *> pure [])
    `alternative` ( do
                      x <- p
                      xs <- manyTill p end
                      pure (x : xs)
                  )


-- | Parses zero or more p separated and optionally ended by sep.
sepEndBy
  :: Ord e
  => Parser e :> es
  => Eff es a
  -> Eff es sep
  -> Eff es [a]
sepEndBy p sep = sepEndBy1 p sep `alternative` pure []


-- | Parses one or more p separated and optionally ended by sep.
sepEndBy1
  :: Ord e
  => Parser e :> es
  => Eff es a
  -> Eff es sep
  -> Eff es [a]
sepEndBy1 p sep = do
  x <- p
  ( do
      _ <- sep
      xs <- sepEndBy p sep
      pure (x : xs)
    )
    `alternative` pure [x]


(<|>)
  :: Ord e
  => Parser e :> es
  => Eff es a
  -> Eff es a
  -> Eff es a
(<|>) = alternative


infixr 1 <|>


label
  :: Parser e :> es
  => NonEmpty Char
  -> Eff es a
  -> Eff es a
label expectation p =
  catchParseError
    p
    ( \e -> do
        throwParseError $
          e {expected = singletonExpectations (ExpectedName expectation)}
    )


(<?>)
  :: Parser e :> es
  => Eff es a
  -> NonEmpty Char
  -> Eff es a
(<?>) = flip label


infix 0 <?>
