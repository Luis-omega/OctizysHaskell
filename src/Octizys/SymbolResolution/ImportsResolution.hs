module Octizys.SymbolResolution.ImportsResolution where

import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import qualified Data.Map as Map
import Effectful (Eff, (:>))
import Effectful.Reader.Static (Reader)
import Octizys.Classes.From (From (from))
import Octizys.Common.Id (ExpressionVariableId)
import Octizys.Common.LogicPath (LogicPath, addAtEnd)
import Octizys.Common.Name (Name)
import Octizys.Cst.SourceInfo (SourceVariable)
import Octizys.Cst.TopItem
  ( ImportModule (ImportModuleAs', ImportModuleUnqualified', alias, items, path)
  )
import Octizys.Scope (ImportsScope)


data ResolutionContext = ResolutionContext'
  { aloneSymbols :: Map Name (NonEmpty LogicPath)
  , qualifierSymbols :: Map LogicPath (NonEmpty LogicPath)
  }
  deriving (Show, Eq, Ord)


emptyResolutionContext :: ResolutionContext
emptyResolutionContext = ResolutionContext' mempty mempty


addQualifierSymbol
  :: LogicPath -> LogicPath -> ResolutionContext -> ResolutionContext
addQualifierSymbol name value rc =
  rc
    { qualifierSymbols =
        Map.insertWith (<>) name (value :| []) rc.qualifierSymbols
    }


addAloneSymbol
  :: Name -> LogicPath -> ResolutionContext -> ResolutionContext
addAloneSymbol name value rc =
  rc
    { aloneSymbols =
        Map.insertWith (<>) name (value :| []) rc.aloneSymbols
    }


addImportModule
  :: ResolutionContext
  -> ImportModule
  -> ResolutionContext
addImportModule rc x =
  case x of
    ImportModuleAs' {path = (_, path), alias} ->
      let asLogicPath = from path
       in case alias of
            Nothing ->
              addQualifierSymbol asLogicPath asLogicPath rc
            Just alias2 ->
              addQualifierSymbol (from alias2) asLogicPath rc
    ImportModuleUnqualified' {path = (_, path), items = Just items} ->
      foldl'
        (add (from path))
        rc
        (from @(NonEmpty Name) items)
      where
        add
          :: LogicPath
          -> ResolutionContext
          -> Name
          -> ResolutionContext
        add p rc2 nam =
          let qualifiedName = addAtEnd nam p
           in addAloneSymbol nam qualifiedName rc2
    ImportModuleUnqualified' {items = Nothing} -> rc


{- | Translates a list of import declarations to a
mapping of symbols and qualifiers to the possible real qualifiers
to lookup.
-}
makeResolutionContext
  :: [ImportModule]
  -> ResolutionContext
makeResolutionContext = foldl' addImportModule emptyResolutionContext


lookupExpressionSourceVariable
  :: ImportsScope
  -> ResolutionContext
  -> SourceVariable
  -> Maybe ExpressionVariableId
lookupExpressionSourceVariable is rc sv = undefined
