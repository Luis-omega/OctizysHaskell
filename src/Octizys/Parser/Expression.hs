module Octizys.Parser.Expression where

import Effectful (Eff, (:>))
import Octizys.Cst.Expression (Expression)
import Octizys.Effects.Parser.Effect (Parser)
import Octizys.Parser.Common (OctizysParseError, uninplemented)


parseExpression :: Parser OctizysParseError :> es => Eff es Expression
parseExpression = uninplemented "Expression Parser not implemented"
