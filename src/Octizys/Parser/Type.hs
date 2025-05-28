module Octizys.Parser.Type
  ( parseType
  , typeHole
  , typeAtom
  , typeAtomNoVar
  ) where

import Control.Monad (forM)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Effectful (Eff, (:>))
import Octizys.Cst.InfoId (InfoId)
import Octizys.Cst.Type
  ( Type
      ( Arrow
      , BoolType
      , IntType
      , Parens
      , Scheme
      , Variable
      , arguments
      , body
      , dot
      , variableId
      , _forall
      )
  )
import qualified Octizys.Cst.Type as Type
import Octizys.Effects.Parser.Combinators
  ( char
  , many
  , some
  , (<?>)
  , (<|>)
  )
import Octizys.Effects.Parser.Effect (Parser)
import Octizys.Effects.SymbolResolution.Effect
  ( SymbolResolution
  , createInformation
  , definitionOfTypeVariable
  , foundTypeVariable
  )
import Octizys.Parser.Common
  ( OctizysParseError
  , between
  , dot
  , forallKeyword
  , identifierParser
  , keyword
  , leftParen
  , rightArrow
  , rightParen
  , token
  )
import qualified Octizys.Parser.Common as Common
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
  tId <- foundTypeVariable "_"
  inf <- createInformation span pre after
  pure Type.Variable {info = inf, variableId = tId}


typeVariable
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es Type
typeVariable = do
  (name, inf, _) <- identifierParser
  tid <- foundTypeVariable name
  pure Variable {info = inf, variableId = tid}


parens
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es Type
parens = do
  (lparen, _type, rparen) <-
    between leftParen rightParen typeParser
  pure Parens {..}


typeAtomNoVar
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es Type
typeAtomNoVar = do
  typeConstantParser
    <|> parens
    <|> typeVariable


-- We don't have type vars yet!
typeAtom
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es Type
typeAtom = typeAtomNoVar


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


typeSchemeParser
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es Type
typeSchemeParser = do
  _forall <- forallKeyword
  paramInfos <- some identifierParser
  arguments <-
    forM
      paramInfos
      ( \(nam, inf, parSpan) -> do
          tvid <- definitionOfTypeVariable nam parSpan
          pure (inf, tvid)
      )
  _dot <- Common.dot
  body <- typeParser
  pure
    Scheme
      { _forall
      , arguments
      , dot = _dot
      , body
      }


typeParser
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es Type
typeParser = typeSchemeParser <|> typeArrowParser
