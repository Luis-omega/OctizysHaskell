{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Octizys.Parser
  ( ParserError
  , Parser
  , parseTop
  , parseExpression
  , parserToEff
  , expressionParser
  , typeParser
  , topParser
  , uninplemented
  , testParser
  , errorBundlePretty
  , Text.Megaparsec.eof
  , moduleParser
  , parseModule
  , sc
  , lexeme
  , symbol
  , charParser
  , keyword
  , makeTypeHole
  , makeExpressionVariable
  , makeExpressionVariableFromSymbol
  , Type
  , TopItem
  , Expression
  , ExpressionVariable (ExpressionVariableC)
  , TypeVariable
  )
where

import Control.Arrow ((<<<))
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Functor (void)
import Effectful (Eff, (:>))
import Effectful.Error.Dynamic (Error, throwError)
import Octizys.Ast
  ( AstError
  , LetDefinition (LetDefinitionC, letDefinition, letName)
  , Symbol
  , makeApplication
  , makeArrow
  , makeBool
  , makeBoolType
  , makeFunction
  , makeIf
  , makeInt
  , makeIntType
  , makeLet
  , makeSymbol
  , makeTopItem
  , symbolToString
  )
import qualified Octizys.Ast as Ast
import Prettyprinter (Pretty (pretty))
import Text.Megaparsec
  ( MonadParsec (lookAhead, takeWhile1P, takeWhileP)
  , ParseErrorBundle
  , Parsec
  , between
  , customFailure
  , empty
  , eof
  , errorBundlePretty
  , many
  , parse
  , runParser
  , some
  , (<?>)
  , (<|>)
  )
import qualified Text.Megaparsec.Byte.Lexer as L
import Text.Megaparsec.Char (char, space1)


data TypeVariable
  = TypeHole
  deriving (Show, Eq, Ord)


instance Pretty TypeVariable where
  pretty TypeHole = pretty "_"


makeTypeHole :: Ast.Type TypeVariable
makeTypeHole = Ast.TypeVar TypeHole


newtype ExpressionVariable
  = ExpressionVariableC Symbol
  deriving (Show, Eq, Ord)


instance Pretty ExpressionVariable where
  pretty (ExpressionVariableC s) = pretty s


makeExpressionVariable
  :: String
  -> Either AstError Expression
makeExpressionVariable s =
  makeExpressionVariableFromSymbol <$> makeSymbol s


makeExpressionVariableFromSymbol
  :: Symbol
  -> Expression
makeExpressionVariableFromSymbol =
  Ast.Variable <<< ExpressionVariableC


type Type = Ast.Type TypeVariable


type Expression =
  Ast.Expression ExpressionVariable TypeVariable


type TopItem =
  Ast.TopItem ExpressionVariable TypeVariable


type ParserError = ParseErrorBundle String AstError


type Parser = Parsec AstError String


-- ==================== Auxiliary Functions =================

uninplemented :: forall a. String -> Parser a
uninplemented s = fail ("Uninplemented " <> s <> " parser")


testParser :: Parser a -> String -> Either ParserError a
testParser p = runParser p "test"


parserToEff :: Error ParserError :> es => Parser a -> String -> Eff es a
parserToEff p s =
  -- TODO : remove this hardcore of "repl"
  case parse p "repl" s of
    Left e -> throwError e
    Right a -> pure a


withPredicate1 :: (a -> Bool) -> String -> Parser a -> Parser a
withPredicate1 predicate errorMsg p = do
  result <- lookAhead p
  if predicate result
    then p
    else fail errorMsg


liftError
  :: forall a e
   . Ord e
  => Parsec e String (Either e a)
  -> Parsec e String a
liftError p = do
  result <- p
  case result of
    Left e -> customFailure e
    Right r -> pure r


-- Espacios opcionales
sc :: Parser ()
sc = L.space space1 empty empty


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc


symbol :: String -> Parser String
symbol = L.symbol sc


charParser :: Char -> Parser ()
charParser = void <<< lexeme <<< char


keyword :: String -> Parser ()
keyword = void <<< symbol


-- ======================= Lexer ===========================

identifierOrKeyword :: Parser Symbol
identifierOrKeyword = lexeme $ do
  _head <- takeWhile1P (Just "identifier start character") isAlpha
  remain <-
    takeWhileP
      (Just "identifier character")
      (\c -> isAlphaNum c || c == '_')
  let full_string = _head <> remain
  liftError $ pure $ makeSymbol full_string


identifierParser :: Parser Symbol
identifierParser =
  withPredicate1
    ( \s ->
        symbolToString s
          `notElem` [ "if"
                    , "then"
                    , "else"
                    , "let"
                    , "in"
                    ]
    )
    "keyword found, expected identifier"
    identifierOrKeyword


colon :: Parser ()
colon = charParser ':'


semicolon :: Parser ()
semicolon = charParser ';'


equal :: Parser ()
equal = charParser '='


lambdaStart :: Parser ()
lambdaStart = charParser '\\'


rightArrow :: Parser ()
rightArrow = keyword "->"


leftParen :: Parser ()
leftParen = charParser '('


rightParen :: Parser ()
rightParen = charParser ')'


leftBrace :: Parser ()
leftBrace = charParser '{'


rightBrace :: Parser ()
rightBrace = charParser '}'


ifKeyword :: Parser ()
ifKeyword = keyword "if"


thenKeyword :: Parser ()
thenKeyword = keyword "then"


elseKeyword :: Parser ()
elseKeyword = keyword "else"


letKeyword :: Parser ()
letKeyword = keyword "let"


inKeyword :: Parser ()
inKeyword = keyword "in"


parens :: Parser a -> Parser a
parens = between leftParen rightParen


braces :: Parser a -> Parser a
braces = between leftBrace rightBrace


-- ======================= Types ===========================

typeIntParser :: Parser (Ast.Type tvar)
typeIntParser = makeIntType <$ symbol "int"


typeBoolParser :: Parser (Ast.Type tvar)
typeBoolParser = makeBoolType <$ symbol "bool"


typeConstantParser :: Parser (Ast.Type tvar)
typeConstantParser = typeIntParser <|> typeBoolParser


typeHole :: Parser Type
typeHole =
  (makeTypeHole <$ lexeme (char '_'))
    <?> "a type variable"


typeAtom :: Parser Type
typeAtom =
  typeConstantParser
    <|> parens typeParser


typeArrowParser :: Parser Type
typeArrowParser = do
  initial <- typeAtom
  remain <- many (symbol "->" >> typeAtom)
  case remain of
    [] -> pure initial
    _ -> (liftError <<< pure <<< makeArrow initial) remain


typeParser :: Parser Type
typeParser = typeArrowParser


-- ======================= Literals ===========================

boolParser :: Parser Expression
boolParser =
  (makeBool True <$ symbol "true")
    <|> (makeBool False <$ symbol "false")


intParser :: Parser (Ast.Expression evars tvars)
intParser =
  lexeme
    ( do
        _head <- takeWhile1P (Just "digit") isDigit
        others <- takeWhileP (Just "digit or _") (\c -> isDigit c || c == '_')
        (pure <<< makeInt) (_head <> others)
    )
    <?> "valid integer"


-- ======================= Expression ===========================

variableParser :: Parser Expression
variableParser =
  makeExpressionVariableFromSymbol <$> identifierParser


functionParser :: Parser Expression
functionParser = do
  lambdaStart
  parameters <-
    some
      ( ExpressionVariableC
          <$> identifierParser
      )
  rightArrow
  liftError
    (makeFunction parameters <$> expressionParser)


atomExpressionParser :: Parser Expression
atomExpressionParser =
  boolParser
    <|> intParser
    <|> functionParser
    <|> parensExpressionParser
    <|> ifParser
    <|> letParser
    -- Keep it at the end, it prevents the capture
    -- of keywords by variableParser
    -- Maybe we should check inside variableParser
    -- but this is a cheap trick
    <|> variableParser


parensExpressionParser :: Parser Expression
parensExpressionParser = parens expressionParser


ifParser :: Parser Expression
ifParser = do
  _ <- ifKeyword
  condition <- expressionParser
  _ <- thenKeyword
  _then <- expressionParser
  _ <- elseKeyword
  makeIf condition _then <$> expressionParser


letDefinitionParser
  :: Parser
      ( LetDefinition ExpressionVariable TypeVariable
      )
letDefinitionParser = do
  name <- ExpressionVariableC <$> identifierParser
  _ <- equal
  expr <- expressionParser
  _ <- semicolon
  pure LetDefinitionC {letName = name, letDefinition = expr}


letParser :: Parser Expression
letParser = do
  _ <- letKeyword
  definitions <- some (letDefinitionParser <?> "a definition")
  _ <- inKeyword
  liftError $
    makeLet definitions <$> expressionParser


applicationParser :: Parser Expression
applicationParser = do
  function <- atomExpressionParser
  arguments <- many atomExpressionParser
  case arguments of
    [] -> pure function
    _ -> case makeApplication function arguments of
      Left e -> customFailure e
      Right x -> pure x


expressionParser :: Parser Expression
expressionParser = applicationParser


parseExpression
  :: Error ParserError :> es => String -> Eff es Expression
parseExpression = parserToEff expressionParser


-- ======================= Top ===========================

topParser :: Parser TopItem
topParser = do
  name <- identifierParser
  _type <-
    (colon >> (typeParser <|> typeHole)) <?> "type signature for definition"
  equal
  braces
    ( makeTopItem name _type <$> expressionParser
    )
    <?> "definition expression "


parseTop :: Error ParserError :> es => String -> Eff es TopItem
parseTop = parserToEff topParser


-- ======================= Module ===========================

-- We don't have modules yet, this is more a `parse
-- a bunch of definitions one after another` right now.
moduleParser :: Parser [TopItem]
moduleParser = sc >> many (topParser <?> "a definition")


parseModule :: Error ParserError :> es => String -> Eff es [TopItem]
parseModule = parserToEff moduleParser
