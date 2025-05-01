module Octizys.Parser.TopItem where

import Effectful (Eff, (:>))
import Octizys.Cst.Expression (Definition)
import Octizys.Cst.TopItem (Module (Module', definitions, lastComments))
import Octizys.Effects.Parser.Combinators (many)
import Octizys.Effects.Parser.Effect (Parser)
import Octizys.Effects.SymbolResolution.Effect
  ( SymbolResolution
  , createInformation
  )
import Octizys.Parser.Common (OctizysParseError, colon, comments)
import Octizys.Parser.Expression (definitionParser)


parseModule
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es Module
parseModule = do
  definitions <- many $ do
    def <- definitionParser
    colonInfo <- colon
    pure (def, colonInfo)
  lastCommentsRaw <- comments
  case lastCommentsRaw of
    [] -> pure Module' {lastComments = Nothing, ..}
    _ ->
      do
        lastCommentsJust <-
          createInformation
            undefined
            lastCommentsRaw
            Nothing
        pure Module' {lastComments = Just lastCommentsJust, ..}


parserDefinitions
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es [Definition]
parserDefinitions = many definitionParser
