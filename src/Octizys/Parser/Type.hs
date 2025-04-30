module Octizys.Parser.Type where

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


data CstError = CstError1 | CstError2


-- * ==================== Auxiliary Functions =================


newtype Symbol = Symbol' Text


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

-- testParser :: Parser a -> Text -> Either ParserError a
-- testParser p = runParser p "test"
--
--
-- parserToEff :: Error ParserError :> es => Parser a -> Text -> Eff es a
-- parserToEff p s =
--   -- TODO : remove this hardcore of "repl"
--   case parse p "repl" s of
--     Left e -> throwError e
--     Right a -> pure a
--
--
-- withPredicate1 :: (a -> Bool) -> Text -> Parser a -> Parser a
-- withPredicate1 predicate errorMsg p = do
--   result <- lookAhead p
--   if predicate result
--     then p
--     else fail errorMsg
--
--
-- liftError
--   :: forall a e
--    . Ord e
--   => Parsec e Text (Either e a)
--   -> Parsec e Text a
-- liftError p = do
--   result <- p
--   case result of
--     Left e -> customFailure e
--     Right r -> pure r
--
--
-- -- Espacios opcionales
-- sc :: Parser ()
-- sc = L.space space1 empty empty
--
--
-- lexeme :: Parser a -> Parser a
-- lexeme = L.lexeme sc
--
--
-- symbol :: Text -> Parser Text
-- symbol = L.symbol sc
--
--
-- charParser :: Char -> Parser ()
-- charParser = void <<< lexeme <<< char
--
--
-- keyword :: Text -> Parser ()
-- keyword = void <<< symbol
--
--
-- -- ======================= Lexer ===========================
--
-- positionFromPos :: SourcePos -> Position
-- positionFromPos s = Position' {line = s.sourceLine, column = s.sourceColumn}
--
--
-- spanFromPos :: SourcePos -> SourcePos -> Span
-- spanFromPos s1 s2 =
--   let start = positionFromPos s1
--       end = positionFromPos s2
--    in Span' {..}
--
--
-- trackSpan :: Parser a -> Parser (a, Span)
-- trackSpan p = do
--   start <- getSourcePos
--   content <- p
--   end <- getSourcePos
--   pure (content, spanFromPos start end)
--
--
-- lineCommentParser :: Parser (Either () Comment)
-- lineCommentParser = do
--   result <- try (trackSpan parseLineComment)
--   case result of
--     Left _ -> pure ()
--     Right (lineComment, span) -> Line {..}
--   where
--     parseLineComment = do
--       _ <- string "--"
--       takeWhileP (!= '\n') (Just "a non line break")
--
-- blockCommentParser :: Parser (Either () Comment)
-- blockCommentParser = do
--   result <- try (trackSpan parseBlockComment)
--   case result of
--     Left _ -> pure ()
--     Right (blockComment, span) -> Block {..}
--   where
--     innerElement = do
--       start <- takeWhileP (!= '-' )
--       second <- try (Just <$> string "-}" ) <|> Nothing
--       case second of
--         Nothing -> (start <> secondPart) <> innerElement
--         Just _ ->  start
--
--     parseBlockComment =
--       -- TODO: handle nested comments.
--       do
--         _ <- string "{-"
--         content <- innerElement
--         _ <- string "-}"
--         content
--
--
--
--
-- identifierOrKeyword :: Parser Symbol
-- identifierOrKeyword = lexeme $ do
--   _head <- takeWhile1P (Just "identifier start character") isAlpha
--   remain <-
--     takeWhileP
--       (Just "identifier character")
--       (\c -> isAlphaNum c || c == '_')
--   let full_string = _head <> remain
--   liftError $ pure $ makeSymbol full_string
--
--
-- identifierParser :: Parser Symbol
-- identifierParser =
--   withPredicate1
--     ( \s ->
--         symbolToString s
--           `notElem` [ "if"
--                     , "then"
--                     , "else"
--                     , "let"
--                     , "in"
--                     ]
--     )
--     "keyword found, expected identifier"
--     identifierOrKeyword
--
--
-- colon :: Parser ()
-- colon = charParser ':'
--
--
-- semicolon :: Parser ()
-- semicolon = charParser ';'
--
--
-- equal :: Parser ()
-- equal = charParser '='
--
--
-- lambdaStart :: Parser ()
-- lambdaStart = charParser '\\'
--
--
-- rightArrow :: Parser ()
-- rightArrow = keyword "->"
--
--
-- leftParen :: Parser ()
-- leftParen = charParser '('
--
--
-- rightParen :: Parser ()
-- rightParen = charParser ')'
--
--
-- leftBrace :: Parser ()
-- leftBrace = charParser '{'
--
--
-- rightBrace :: Parser ()
-- rightBrace = charParser '}'
--
--
-- ifKeyword :: Parser ()
-- ifKeyword = keyword "if"
--
--
-- thenKeyword :: Parser ()
-- thenKeyword = keyword "then"
--
--
-- elseKeyword :: Parser ()
-- elseKeyword = keyword "else"
--
--
-- letKeyword :: Parser ()
-- letKeyword = keyword "let"
--
--
-- inKeyword :: Parser ()
-- inKeyword = keyword "in"
--
--
-- parens :: Parser a -> Parser a
-- parens = between leftParen rightParen
--
--
-- braces :: Parser a -> Parser a
-- braces = between leftBrace rightBrace
--
--
-- -- ======================= Types ===========================
--
-- typeIntParser :: Parser (Ast.Type tvar)
-- typeIntParser = makeIntType <$ symbol "int"
--
--
-- typeBoolParser :: Parser (Ast.Type tvar)
-- typeBoolParser = makeBoolType <$ symbol "bool"
--
--
-- typeConstantParser :: Parser (Ast.Type tvar)
-- typeConstantParser = typeIntParser <|> typeBoolParser
--
--
-- typeHole :: Parser Type
-- typeHole =
--   (makeTypeHole <$ lexeme (char '_'))
--     <?> "a type variable"
--
--
-- typeAtom :: Parser Type
-- typeAtom =
--   typeConstantParser
--     <|> parens typeParser
--
--
-- typeArrowParser :: Parser Type
-- typeArrowParser = do
--   initial <- typeAtom
--   remain <- many (symbol "->" >> typeAtom)
--   case remain of
--     [] -> pure initial
--     _ -> (liftError <<< pure <<< makeArrow initial) remain
--
--
-- typeParser :: Parser Type
-- typeParser = typeArrowParser
--
--
-- -- ======================= Literals ===========================
--
-- boolParser :: Parser Expression
-- boolParser =
--   (makeBool True <$ symbol "true")
--     <|> (makeBool False <$ symbol "false")
--
--
-- intParser :: Parser (Ast.Expression evars tvars)
-- intParser =
--   lexeme
--     ( do
--         _head <- takeWhile1P (Just "digit") isDigit
--         others <- takeWhileP (Just "digit or _") (\c -> isDigit c || c == '_')
--         (pure <<< makeInt) (_head <> others)
--     )
--     <?> "valid integer"
--
--
-- -- ======================= Expression ===========================
--
-- variableParser :: Parser Expression
-- variableParser =
--   makeExpressionVariableFromSymbol <$> identifierParser
--
--
-- functionParser :: Parser Expression
-- functionParser = do
--   lambdaStart
--   parameters <-
--     some
--       ( ExpressionVariableC
--           <$> identifierParser
--       )
--   rightArrow
--   liftError
--     (makeFunction parameters <$> expressionParser)
--
--
-- atomExpressionParser :: Parser Expression
-- atomExpressionParser =
--   boolParser
--     <|> intParser
--     <|> functionParser
--     <|> parensExpressionParser
--     <|> ifParser
--     <|> letParser
--     -- Keep it at the end, it prevents the capture
--     -- of keywords by variableParser
--     -- Maybe we should check inside variableParser
--     -- but this is a cheap trick
--     <|> variableParser
--
--
-- parensExpressionParser :: Parser Expression
-- parensExpressionParser = parens expressionParser
--
--
-- ifParser :: Parser Expression
-- ifParser = do
--   _ <- ifKeyword
--   condition <- expressionParser
--   _ <- thenKeyword
--   _then <- expressionParser
--   _ <- elseKeyword
--   makeIf condition _then <$> expressionParser
--
--
-- letDefinitionParser
--   :: Parser
--       ( LetDefinition ExpressionVariable TypeVariable
--       )
-- letDefinitionParser = do
--   name <- ExpressionVariableC <$> identifierParser
--   _ <- equal
--   expr <- expressionParser
--   _ <- semicolon
--   pure LetDefinitionC {letName = name, letDefinition = expr}
--
--
-- letParser :: Parser Expression
-- letParser = do
--   _ <- letKeyword
--   definitions <- some (letDefinitionParser <?> "a definition")
--   _ <- inKeyword
--   liftError $
--     makeLet definitions <$> expressionParser
--
--
-- applicationParser :: Parser Expression
-- applicationParser = do
--   function <- atomExpressionParser
--   arguments <- many atomExpressionParser
--   case arguments of
--     [] -> pure function
--     _ -> case makeApplication function arguments of
--       Left e -> customFailure e
--       Right x -> pure x
--
--
-- expressionParser :: Parser Expression
-- expressionParser = applicationParser
--
--
-- parseExpression
--   :: Error ParserError :> es => Text -> Eff es Expression
-- parseExpression = parserToEff expressionParser
--
--
-- -- ======================= Top ===========================
--
-- topParser :: Parser TopItem
-- topParser = do
--   name <- identifierParser
--   _type <-
--     (colon >> (typeParser <|> typeHole)) <?> "type signature for definition"
--   equal
--   braces
--     ( makeTopItem name _type <$> expressionParser
--     )
--     <?> "definition expression "
--
--
-- parseTop :: Error ParserError :> es => Text -> Eff es TopItem
-- parseTop = parserToEff topParser
--
--
-- -- ======================= Module ===========================
--
-- -- We don't have modules yet, this is more a `parse
-- -- a bunch of definitions one after another` right now.
-- moduleParser :: Parser [TopItem]
-- moduleParser = sc >> many (topParser <?> "a definition")
--
--
-- parseModule :: Error ParserError :> es => Text -> Eff es [TopItem]
-- parseModule = parserToEff moduleParser
--
