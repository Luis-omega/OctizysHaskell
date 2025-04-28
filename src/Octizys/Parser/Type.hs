module Octizys.Parser.Type where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Effectful (Eff, (:>))
import Effectful.State.Static.Local (State, modify)
import Octizys.Cst.Expression
  ( ExpressionVariableId
  , freshExpressionVariableId
  )
import Octizys.Cst.InfoId (InfoId)
import Octizys.Cst.Type (TypeVariableId, freshTypeVariableId)
import Octizys.Effects.Generator.Effect (Generator, generate)
import Octizys.Effects.Parser.Combinators (Span, errorMessage)
import Octizys.Effects.Parser.Effect (Parser)

import Prelude hiding (span)


data CstError = CstError1 | CstError2


-- * ==================== Auxiliary Functions =================


newtype Symbol = Symbol' Text


uninplemented :: forall a e es. Parser e :> es => Text -> Eff es a
uninplemented s = errorMessage ("Uninplemented " <> s <> " parser")


newtype LineComment = LineComment'
  { content :: Text
  }
  deriving (Show, Eq, Ord)


newtype BlockComment = BlockComment'
  { content :: [LineComment]
  }
  deriving (Show, Eq, Ord)


data Comment
  = Line {lineComment :: LineComment, span :: Span}
  | Block {blockComment :: BlockComment, span :: Span}
  deriving (Show, Eq, Ord)


data SourceInfo = SourceInfo'
  { span :: Span
  , preComments :: [Comment]
  , afterComment :: Maybe Comment
  }
  deriving (Show, Eq, Ord)


registerInfo
  :: State (Map InfoId SourceInfo) :> es
  => Generator InfoId :> es
  => Span
  -> [Comment]
  -> Maybe Comment
  -> Eff es InfoId
registerInfo span preComments afterComment = do
  newId <- generate
  modify $ \s -> Map.insert newId (SourceInfo' {..}) s
  pure newId


data SourceTypeVariableInfo = SourceTypeVariableInfo'
  { name :: Maybe Text
  , variableId :: TypeVariableId
  , definitionSpan :: Maybe Span
  }
  deriving (Show, Eq, Ord)


registerTypeVariable
  :: State (Map TypeVariableId SourceTypeVariableInfo) :> es
  => Generator TypeVariableId :> es
  => Maybe Text
  -> Maybe Span
  -> Eff es TypeVariableId
registerTypeVariable name definitionSpan = do
  variableId <- freshTypeVariableId
  modify $ \s -> Map.insert variableId (SourceTypeVariableInfo' {..}) s
  pure variableId


data SourceExpressionVariableInfo = SourceExpressionVariableInfo'
  { name :: Text
  , variableId :: ExpressionVariableId
  , definitionSpan :: Maybe Span
  , typeId :: TypeVariableId
  }
  deriving (Show, Eq, Ord)


registerExpressionVariable
  :: State (Map ExpressionVariableId SourceExpressionVariableInfo) :> es
  => State (Map TypeVariableId SourceTypeVariableInfo) :> es
  => Generator ExpressionVariableId :> es
  => Generator TypeVariableId :> es
  => Text
  -> Maybe Span
  -> Eff es ExpressionVariableId
registerExpressionVariable name definitionSpan = do
  variableId <- freshExpressionVariableId
  -- Type variables don't have name yet as user can't create them!
  typeId <- registerTypeVariable Nothing definitionSpan
  modify $ \s -> Map.insert variableId (SourceExpressionVariableInfo' {..}) s
  pure variableId

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
--         Nothing -> (start <> seconPart) <> innerElement
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
