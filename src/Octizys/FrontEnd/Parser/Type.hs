module Octizys.FrontEnd.Parser.Type
  ( parseType
  , typeHole
  , typeAtom
  , typeAtomNoVar
  ) where

import Control.Arrow ((<<<))
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
import Octizys.Classes.From (From (from))
import Octizys.Common.Id (SymbolOriginInfo)
import Octizys.Common.Name (Name, makeName)
import Octizys.FrontEnd.Cst.SourceInfo
  ( SourceInfo
  , SourceVariable
  , makeSourceInfo
  )
import Octizys.FrontEnd.Cst.Type (Type)
import qualified Octizys.FrontEnd.Cst.Type as Type
import Octizys.FrontEnd.Parser.Common
  ( between
  , keyword
  , leftParen
  , rightArrow
  , rightParen
  , sourceVariableParser
  , token
  )
import Octizys.FrontEnd.Parser.Error
  ( OctizysParseError (CantParseName)
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
typeIntParser = (Type.TInt <<< Type.IntType') <$> keyword "Int"


typeBoolParser
  :: Parser OctizysParseError :> es
  => Eff es (Type SourceVariable)
typeBoolParser = (Type.TBool <<< Type.BoolType') <$> keyword "Bool"


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
  pure $
    Type.TVariable $
      Type.Variable'
        { info = inf
        , variable =
            from $ from @SymbolOriginInfo ([] @Name, name)
        }


typeVariable
  :: Parser OctizysParseError :> es
  => Eff es (Type SourceVariable)
typeVariable = do
  (variable, info) <- sourceVariableParser
  pure $
    Type.TVariable $
      Type.Variable' {info, variable}


parens
  :: Parser OctizysParseError :> es
  => Eff es (Type SourceVariable)
parens = do
  (lparen, _type, rparen) <-
    between leftParen rightParen typeParser
  pure $ Type.TParens $ Type.Parens' {..}


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
    (r : emain) -> pure $ from $ Type.Arrow' {start, remain = r :| emain}


typeParser
  :: Parser OctizysParseError :> es
  => Eff es (Type SourceVariable)
typeParser = typeArrowParser
