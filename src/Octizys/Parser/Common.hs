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
  ( errorMessage
  , getPosition
  , item
  , many
  , optional
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
import Data.Functor (void)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as Text
import Prettyprinter (Pretty (pretty))
import Prelude hiding (span)

-- * ==================== Auxiliary Functions =================


newtype Symbol = Symbol' {unSymbol:: Text}


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
comments = many comment


token
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es a
  -> Eff es (a, InfoId)
token p = do
  pre <- preC
  p1 <- getPosition
  result <- p
  p2 <- getPosition
  after <- afterC
  sourceId <- createInformation (Span' {start = p1, end = p2}) pre after
  pure (result, sourceId)
  where
    preC = many $ do
      comment <* skipSimpleSpaces
    afterC =
      optional (skipSimpleSpaces *> (parseLineComment <* skipSimpleSpaces))
