{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Octizys.FrontEnd.Parser.Common where

import Data.Text (Text)
import Effectful (Eff, (:>))
import EffectfulParserCombinators.Combinators
  ( char
  , errorCustom
  , errorMessage
  , getPosition
  , hidden
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
import EffectfulParserCombinators.Effect
  ( Parser
  )
import EffectfulParserCombinators.Span (Span (Span', end, start))
import Octizys.FrontEnd.Cst.Comment
  ( BlockComment (BlockComment')
  , Comment (Block, Line, blockComment, lineComment, span)
  , LineComment (LineComment')
  )

import Control.Arrow ((<<<))
import Data.Char (isAlpha, isAlphaNum)
import Data.Functor (void)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as Text
import Octizys.Classes.From (From (from))
import Octizys.Common.Name (Name, makeName)
import Octizys.FrontEnd.Cst.SourceInfo
  ( SourceInfo
  , SourceVariable
  , makeSourceInfo
  )
import Prettyprinter (Pretty (pretty))
import Prelude hiding (span)

import Data.Aeson (ToJSON)
import GHC.Generics (Generic, Generically (..))
import Octizys.Common.Id (SymbolOriginInfo)


-- * ==================== Auxiliary Functions =================


newtype Symbol = Symbol' {unSymbol :: Text}


uninplemented :: forall a e es. Parser e :> es => Text -> Eff es a
uninplemented s = errorMessage ("Uninplemented " <> s <> " parser")


data OctizysParseError
  = CantParseName Text
  | EmptyImportList
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically OctizysParseError


instance Pretty OctizysParseError where
  pretty (CantParseName str) =
    pretty @Text
      "A bug, we parsed a identifier but is not a valid identifier: "
      <> pretty str
  pretty EmptyImportList = pretty @Text "All unqualified imports must provide a list of imports."


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
  void $ hidden $ text "--"
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
  void $ hidden (text "{-")
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
  pre <- hidden comments
  p1 <- getPosition
  result <- p
  p2 <- getPosition
  after <- hidden afterC
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


identifierOrKeywordRaw
  :: Parser OctizysParseError :> es
  => Eff es Text
identifierOrKeywordRaw = do
  _head <- takeWhile1P (Just "identifier start character") isAlpha
  remain <-
    takeWhileP
      (\c -> isAlphaNum c || c == '_')
  let full_string = _head <> remain
  pure full_string


identifierOrKeyword
  :: Parser OctizysParseError :> es
  => Eff es (Text, (Span, [Comment], Maybe Comment))
identifierOrKeyword = token identifierOrKeywordRaw


isNotKeyword :: Text -> Bool
isNotKeyword s =
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
              , "forall"
              , "import"
              , "as"
              , "unqualified"
              ]


identifierParser
  :: Parser OctizysParseError :> es
  => Eff es (Text, SourceInfo, Span)
identifierParser = do
  (iden, (span, pre, after)) <-
    try $
      withPredicate1
        ( \(s, _) -> isNotKeyword s
        )
        "keyword found expected identifier"
        identifierOrKeyword
  let sourceInfo = makeSourceInfo span pre after
  pure (iden, sourceInfo, span)


nameParser
  :: Parser OctizysParseError :> es
  => Eff es (Name, SourceInfo)
nameParser = do
  (iden, info, _) <- identifierParser
  name <-
    maybe
      (errorCustom $ CantParseName iden)
      pure
      (makeName iden)
  pure (name, info)


localVariable
  :: Parser OctizysParseError :> es
  => Eff es (SourceVariable, SourceInfo)
localVariable = do
  (name, nameInfo) <- nameParser
  pure (from $ from @SymbolOriginInfo ([] @Name, name), nameInfo)


sourceVariableParserRaw
  :: Parser OctizysParseError :> es
  => Eff es SourceVariable
sourceVariableParserRaw = do
  nameRaw <-
    try $
      withPredicate1
        isNotKeyword
        "keyword found expected identifier"
        identifierOrKeywordRaw
  name <- toName nameRaw
  names <- many $ do
    _ <- moduleSeparator <?> ('m' :| "odule separator")
    localName <-
      try $
        withPredicate1
          isNotKeyword
          "keyword found expected identifier"
          identifierOrKeywordRaw
    toName localName
  let
    variable = case reverse names of
      [] -> from ([] @Name, name)
      (realName : others) ->
        from (name : reverse others, realName)
  pure
    variable
  where
    toName x = maybe (errorCustom $ CantParseName x) pure (makeName x)


sourceVariableParser
  :: Parser OctizysParseError :> es
  => Eff es (SourceVariable, SourceInfo)
sourceVariableParser = do
  (var, (span, pre, after)) <- token sourceVariableParserRaw
  let sourceInfo = makeSourceInfo span pre after
  pure (var, sourceInfo)


tokenAndregister
  :: Parser OctizysParseError :> es
  => Eff es a
  -> Eff es (a, SourceInfo)
tokenAndregister p = do
  (value, (span, pre, after)) <- token p
  let info = makeSourceInfo span pre after
  pure (value, info)


punctuationChar
  :: Parser OctizysParseError :> es
  => Char
  -> Eff es SourceInfo
punctuationChar c = do
  (_, info) <- tokenAndregister $ char c
  pure info


keyword
  :: Parser OctizysParseError :> es
  => Text
  -> Eff es SourceInfo
keyword c = do
  (_, info) <- tokenAndregister $ text c
  pure info


colon
  :: Parser OctizysParseError :> es
  => Eff es SourceInfo
colon = punctuationChar ':'


semicolon
  :: Parser OctizysParseError :> es
  => Eff es SourceInfo
semicolon = punctuationChar ';'


equal
  :: Parser OctizysParseError :> es
  => Eff es SourceInfo
equal = punctuationChar '='


lambdaStart
  :: Parser OctizysParseError :> es
  => Eff es SourceInfo
lambdaStart = punctuationChar '\\'


moduleSeparator
  :: Parser OctizysParseError :> es
  => Eff es SourceInfo
moduleSeparator = punctuationChar '/'


rightArrow
  :: Parser OctizysParseError :> es
  => Eff es SourceInfo
rightArrow = keyword "->"


turnstile
  :: Parser OctizysParseError :> es
  => Eff es SourceInfo
turnstile = keyword "|-"


comma
  :: Parser OctizysParseError :> es
  => Eff es SourceInfo
comma = punctuationChar ','


dot
  :: Parser OctizysParseError :> es
  => Eff es SourceInfo
dot = punctuationChar '.'


leftParen
  :: Parser OctizysParseError :> es
  => Eff es SourceInfo
leftParen = punctuationChar '('


rightParen
  :: Parser OctizysParseError :> es
  => Eff es SourceInfo
rightParen = punctuationChar ')'


leftBrace
  :: Parser OctizysParseError :> es
  => Eff es SourceInfo
leftBrace = punctuationChar '{'


rightBrace
  :: Parser OctizysParseError :> es
  => Eff es SourceInfo
rightBrace = punctuationChar '}'


ifKeyword
  :: Parser OctizysParseError :> es
  => Eff es SourceInfo
ifKeyword = keyword "if"


thenKeyword
  :: Parser OctizysParseError :> es
  => Eff es SourceInfo
thenKeyword = keyword "then"


elseKeyword
  :: Parser OctizysParseError :> es
  => Eff es SourceInfo
elseKeyword = keyword "else"


letKeyword
  :: Parser OctizysParseError :> es
  => Eff es SourceInfo
letKeyword = keyword "let"


inKeyword
  :: Parser OctizysParseError :> es
  => Eff es SourceInfo
inKeyword = keyword "in"


forallKeyword
  :: Parser OctizysParseError :> es
  => Eff es SourceInfo
forallKeyword = keyword "forall"


importKeyword
  :: Parser OctizysParseError :> es
  => Eff es SourceInfo
importKeyword = keyword "import"


asKeyword
  :: Parser OctizysParseError :> es
  => Eff es SourceInfo
asKeyword = keyword "as"


unqualifiedKeyword
  :: Parser OctizysParseError :> es
  => Eff es SourceInfo
unqualifiedKeyword = keyword "unqualified"


between
  :: Parser OctizysParseError :> es
  => Eff es SourceInfo
  -> Eff es SourceInfo
  -> Eff es a
  -> Eff es (SourceInfo, a, SourceInfo)
between open close p = do
  o <- open
  ps <- p
  c <- close
  pure (o, ps, c)


-- TODO : remove the reversions of list here, it can be done
-- by using a list accumulator
trailingList
  :: Parser OctizysParseError :> es
  => Eff es SourceInfo
  -> Eff es a
  -> Eff es ([(a, SourceInfo)], Maybe (a, Maybe SourceInfo))
trailingList sep p = do
  items <- many $ do
    a <- try p
    s <- sep
    pure (a, s)
  case reverse items of
    [] -> do
      lastI <- optional p
      case lastI of
        Just it -> pure ([], Just (it, Nothing))
        Nothing -> pure ([], Nothing)
    ((lastItem, lastComma) : remain) -> do
      realLastItem <- optional p
      case realLastItem of
        Just it -> pure (items, Just (it, Nothing))
        Nothing ->
          pure
            ( reverse remain
            , Just (lastItem, Just lastComma)
            )
