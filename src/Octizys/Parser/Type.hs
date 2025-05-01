module Octizys.Parser.Type (parseType, typeHole, typeAtom) where

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
  , rightArrow
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
  infoArrow <- rightArrow
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
