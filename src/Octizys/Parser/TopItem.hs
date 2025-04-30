module Octizys.Parser.TopItem where

import Effectful (Eff, (:>))
import Octizys.Cst.TopItem (Module)
import Octizys.Effects.Parser.Effect (Parser)
import Octizys.Parser.Common (OctizysParseError, uninplemented)


parseModule :: Parser OctizysParseError :> es => Eff es Module
parseModule = uninplemented "Top Item Parser not implemented"
