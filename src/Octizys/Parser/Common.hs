module Octizys.Parser.Common where

import Data.Text (Text)
import Effectful (Eff, (:>))
import Octizys.Cst.Comment
  ( BlockComment (BlockComment')
  , Comment (Block, Line, blockComment, lineComment, span)
  , LineComment (LineComment')
  )
import Octizys.Cst.InfoId (InfoId)
import Octizys.Cst.Span (Span (Span', end, start))
import Octizys.Effects.Parser.Combinators
  ( char
  , errorMessage
  , getPosition
  , item
  , many
  , optional
  , takeWhile1P
  , takeWhileP
  , text
  , try
  , (<?>)
  , (<|>)
  )
import Octizys.Effects.Parser.Effect
  ( Parser
  )
import Octizys.Effects.SymbolResolution.Effect
  ( SymbolResolution
  , createInformation
  )

import Control.Arrow ((<<<))
import Data.Char (isAlpha, isAlphaNum)
import Data.Functor (void)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as Text
import Prettyprinter (Pretty (pretty))
import Prelude hiding (span)


-- * ==================== Auxiliary Functions =================


newtype Symbol = Symbol' {unSymbol :: Text}


uninplemented :: forall a e es. Parser e :> es => Text -> Eff es a
uninplemented s = errorMessage ("Uninplemented " <> s <> " parser")


data OctizysParseError = Err1 | Err2
  deriving (Show, Eq, Ord)


instance Pretty OctizysParseError where
  pretty Err1 = pretty @String "Err1"
  pretty Err2 = pretty @String "Err2"


skipSimpleSpaces
  :: Parser e :> es
  => Eff es ()
skipSimpleSpaces = void $ takeWhileP (`elem` [' ', '\t', '\n', '\r'])


{- | Parses the same as `p` but it also gets the
span for `p`.
-}
parseWithSpan
  :: Parser e :> es
  => Eff es a
  -> Eff es (a, Span)
parseWithSpan p = do
  start <- getPosition
  result <- p
  end <- getPosition
  pure (result, Span' {start, end})


parseSkipSimpleSpacesAtEnd
  :: Parser e :> es
  => Eff es a
  -> Eff es a
parseSkipSimpleSpacesAtEnd p = p <* skipSimpleSpaces


-- TODO: maybe we need to subtract 1 to the final position?

{- | Parses the item, skips spaces at end, and gets the
span of the item.
To be used for parsing of commentaries.
-}
commentToken
  :: Parser e :> es
  => NonEmpty Char
  -> Eff es a
  -> Eff es (a, Span)
commentToken name p = (parseSkipSimpleSpacesAtEnd <<< parseWithSpan) p <?> name


{- | Parses a simple line comments, it only fails
if no comment is found.
-}
innerLineComment
  :: Parser OctizysParseError :> es
  => Eff es LineComment
innerLineComment = do
  void $ text "--"
  content <- takeWhileP ('\n' /=)
  pure $ LineComment' content


parseLineComment
  :: Parser OctizysParseError :> es
  => Eff es Comment
parseLineComment = do
  -- TODO: typo signals `ine` as an error...
  (inner, span) <- commentToken ('l' :| "ine comment") innerLineComment
  pure Line {lineComment = inner, span = span}


{- | Parses a simple line comments, it only fails
if no comment is found.
-}
innerBlockComment
  :: Parser OctizysParseError :> es
  => Eff es BlockComment
innerBlockComment = do
  void $ text "{-"
  rawText <- parseInner
  let commentLines = LineComment' <$> Text.lines rawText
  pure $ BlockComment' commentLines
  where
    parseInner = do
      pre <- takeWhileP ('-' /=)
      isEnd <- try (Just <$> text "-}") <|> pure Nothing
      case isEnd of
        Just _ -> pure pre
        Nothing -> do
          c <- item
          rest <- parseInner
          pure (pre <> Text.singleton c <> rest)


parseBlockComment
  :: Parser OctizysParseError :> es
  => Eff es Comment
parseBlockComment = do
  (inner, span) <- commentToken ('b' :| "lock comment") innerBlockComment
  pure Block {blockComment = inner, span = span}


comment
  :: Parser OctizysParseError :> es
  => Eff es Comment
comment = parseLineComment <|> parseBlockComment


comments
  :: Parser OctizysParseError :> es
  => Eff es [Comment]
comments = many $ do
  comment <* skipSimpleSpaces


token
  :: Parser OctizysParseError :> es
  => Eff es a
  -> Eff es (a, (Span, [Comment], Maybe Comment))
token p = do
  pre <- comments
  p1 <- getPosition
  result <- p
  p2 <- getPosition
  after <- afterC
  pure (result, (Span' {start = p1, end = p2}, pre, after))
  where
    afterC =
      optional (try (skipSimpleSpaces *> parseLineComment))
        <* skipSimpleSpaces


{- | Takes a parser and try to apply it, then
if successful, it applies the given predicate
and if True returns the result, otherwise fails with the
given message.
-}
withPredicate1
  :: Parser OctizysParseError :> es
  => (a -> Bool)
  -> Text
  -> Eff es a
  -> Eff es a
withPredicate1 predicate errorMsg p = do
  result <- try p
  if predicate result
    then pure result
    else errorMessage errorMsg


identifierOrKeyword
  :: Parser OctizysParseError :> es
  => Eff es (Text, (Span, [Comment], Maybe Comment))
identifierOrKeyword = token $ do
  _head <- takeWhile1P (Just "identifier start character") isAlpha
  remain <-
    takeWhileP
      (\c -> isAlphaNum c || c == '_')
  let full_string = _head <> remain
  pure full_string


identifierParser
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es (Text, InfoId, Span)
identifierParser = do
  (iden, (span, pre, after)) <-
    try $
      withPredicate1
        ( \(s, _) ->
            s
              `notElem` [ "if"
                        , "then"
                        , "else"
                        , "let"
                        , "in"
                        , "Int"
                        , "True"
                        , "False"
                        , "Bool"
                        ]
        )
        "keyword found expected identifier"
        identifierOrKeyword
  sourceInfo <- createInformation span pre after
  pure (iden, sourceInfo, span)


tokenAndregister
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es a
  -> Eff es (a, InfoId)
tokenAndregister p = do
  (value, (span, pre, after)) <- token p
  info <- createInformation span pre after
  pure (value, info)


punctuationChar
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Char
  -> Eff es InfoId
punctuationChar c = do
  (_, info) <- tokenAndregister $ char c
  pure info


keyword
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Text
  -> Eff es InfoId
keyword c = do
  (_, info) <- tokenAndregister $ text c
  pure info


colon
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es InfoId
colon = punctuationChar ':'


semicolon
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es InfoId
semicolon = punctuationChar ';'


equal
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es InfoId
equal = punctuationChar '='


lambdaStart
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es InfoId
lambdaStart = punctuationChar '\\'


rightArrow
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es InfoId
rightArrow = keyword "->"


leftParen
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es InfoId
leftParen = punctuationChar '('


rightParen
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es InfoId
rightParen = punctuationChar ')'


leftBrace
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es InfoId
leftBrace = punctuationChar '{'


rightBrace
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es InfoId
rightBrace = punctuationChar '}'


ifKeyword
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es InfoId
ifKeyword = keyword "if"


thenKeyword
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es InfoId
thenKeyword = keyword "then"


elseKeyword
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es InfoId
elseKeyword = keyword "else"


letKeyword
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es InfoId
letKeyword = keyword "let"


inKeyword
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es InfoId
inKeyword = keyword "in"


between
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es InfoId
  -> Eff es InfoId
  -> Eff es a
  -> Eff es (InfoId, a, InfoId)
between open close p = do
  o <- open
  ps <- p
  c <- close
  pure (o, ps, c)
