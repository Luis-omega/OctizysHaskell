module Octizys.Parser.Type (parseType, typeHole) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Effectful (Eff, (:>))
import Octizys.Cst.InfoId (InfoId)
import Octizys.Cst.Type
  ( Type
      ( Arrow
      , BoolType
      , IntType
      , Parens
      , variableId
      )
  )
import qualified Octizys.Cst.Type as Type
import Octizys.Effects.Parser.Combinators
  ( char
  , many
  , (<?>)
  , (<|>)
  )
import Octizys.Effects.Parser.Effect (Parser)
import Octizys.Effects.SymbolResolution.Effect
  ( SymbolResolution
  , createInformation
  , createTypeVariable
  )
import Octizys.Parser.Common
  ( OctizysParseError
  , between
  , keyword
  , leftParen
  , rightParen
  , token
  )
import Prelude hiding (span)


parseType
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es Type
parseType = typeParser


-- ======================= Types ===========================

typeIntParser
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es Type
typeIntParser = IntType <$> keyword "Int"


typeBoolParser
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es Type
typeBoolParser = BoolType <$> keyword "Bool"


typeConstantParser
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es Type
typeConstantParser = typeIntParser <|> typeBoolParser


typeHole
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es Type
typeHole = do
  (_, (span, pre, after)) <-
    token (char '_')
      <?> ('a' :| " type variable")
  tId <- createTypeVariable Nothing (Just span)
  inf <- createInformation span pre after
  pure Type.Variable {info = inf, variableId = tId}


parens
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es Type
parens = do
  (lparen, _type, rparen) <-
    between leftParen rightParen typeParser
  pure Parens {..}


typeAtom
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es Type
typeAtom =
  -- We don't have type vars yet!
  typeConstantParser
    <|> parens


typeArrowAndType
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es (InfoId, Type)
typeArrowAndType = do
  infoArrow <- keyword "->"
  _type <- typeAtom
  pure (infoArrow, _type)


typeArrowParser
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es Type
typeArrowParser = do
  start <- typeAtom
  remain <- many typeArrowAndType
  case remain of
    [] -> pure start
    (r : emain) -> pure Arrow {start, remain = r :| emain}


typeParser
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es Type
typeParser = typeArrowParser

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
