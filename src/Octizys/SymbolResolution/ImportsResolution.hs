module Octizys.SymbolResolution.ImportsResolution where

import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Effectful (Eff, (:>))
import Effectful.Reader.Static (Reader)
import Language.Haskell.TH (Name)
import Octizys.Common.LogicPath (LogicPath)
import Octizys.Cst.TopItem (ImportModule)
import Octizys.Scope (ImportsScope)


data ResolutionContext = ResolutionContext'
  { aloneSymbols :: Map Name (NonEmpty Int)
  , qualifierSymbols :: Map LogicPath (NonEmpty Int)
  }


solveQualifiedImports
  :: Reader ImportsScope :> es
  => [ImportModule]
  -> Eff es ImportsScope
solveQualifiedImports = undefined


{- | Translates a list of import declarations to a
mapping of symbols and qualifiers to the possible real qualifiers
to lookup.
-}
makeImportsMap
  :: [ImportModule]
  -> Eff es ResolutionContext
makeImportsMap = undefined
