module Octizys.Parser.Type
  ( parseType
  , typeHole
  , typeAtom
  , typeAtomNoVar
  ) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Effectful (Eff, (:>))
import EffectfulParserCombinators.Combinators
  ( char
  , errorCustom
  , many
  , (<?>)
  , (<|>)
  )
import EffectfulParserCombinators.Effect (Parser)
import Octizys.Common.Name (makeName)
import Octizys.Cst.SourceInfo
  ( SourceInfo
  , SourceVariable (SourceVariable', name, qualifier)
  , makeSourceInfo
  )
import Octizys.Cst.Type
  ( Type
      ( Arrow
      , BoolType
      , IntType
      , Parens
      , TVariable
      , info
      , variable
      )
  )
import qualified Octizys.Cst.Type as Type
import Octizys.Parser.Common
  ( OctizysParseError (CantParseName)
  , between
  , keyword
  , leftParen
  , rightArrow
  , rightParen
  , sourceVariableParser
  , token
  )
import Prelude hiding (span)


parseType
  :: Parser OctizysParseError :> es
  => Eff es (Type SourceVariable)
parseType = typeParser


-- ======================= Types ===========================

typeIntParser
  :: Parser OctizysParseError :> es
  => Eff es (Type SourceVariable)
typeIntParser = IntType <$> keyword "Int"


typeBoolParser
  :: Parser OctizysParseError :> es
  => Eff es (Type SourceVariable)
typeBoolParser = BoolType <$> keyword "Bool"


typeConstantParser
  :: Parser OctizysParseError :> es
  => Eff es (Type SourceVariable)
typeConstantParser = typeIntParser <|> typeBoolParser


typeHole
  :: Parser OctizysParseError :> es
  => Eff es (Type SourceVariable)
typeHole = do
  (_, (span, pre, after)) <-
    token (char '_')
      <?> ('a' :| " type hole")
  let inf = makeSourceInfo span pre after
  name <- maybe (errorCustom $ CantParseName "_") pure (makeName "_")
  pure
    Type.TVariable
      { info = inf
      , variable =
          SourceVariable'
            { qualifier = Nothing
            , name
            }
      }


typeVariable
  :: Parser OctizysParseError :> es
  => Eff es (Type SourceVariable)
typeVariable = do
  (variable, info) <- sourceVariableParser
  pure
    TVariable {info, variable}


parens
  :: Parser OctizysParseError :> es
  => Eff es (Type SourceVariable)
parens = do
  (lparen, _type, rparen) <-
    between leftParen rightParen typeParser
  pure Parens {..}


typeAtomNoVar
  :: Parser OctizysParseError :> es
  => Eff es (Type SourceVariable)
typeAtomNoVar =
  typeConstantParser
    <|> parens
    <|> typeVariable


-- We don't have type vars yet!
typeAtom
  :: Parser OctizysParseError :> es
  => Eff es (Type SourceVariable)
typeAtom = typeAtomNoVar


typeArrowAndType
  :: Parser OctizysParseError :> es
  => Eff es (SourceInfo, Type SourceVariable)
typeArrowAndType = do
  infoArrow <- rightArrow
  _type <- typeAtom
  pure (infoArrow, _type)


typeArrowParser
  :: Parser OctizysParseError :> es
  => Eff es (Type SourceVariable)
typeArrowParser = do
  start <- typeAtom
  remain <- many typeArrowAndType
  case remain of
    [] -> pure start
    (r : emain) -> pure Arrow {start, remain = r :| emain}


typeParser
  :: Parser OctizysParseError :> es
  => Eff es (Type SourceVariable)
typeParser = typeArrowParser
