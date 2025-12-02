{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use (,)" #-}
module Octizys.SymbolResolution.ImportsResolution
  ( ImportsContext
  , ImportErrors
  , ImportError
  , ImportWarning
  , makeImportsContext
  , lookupExpressionSourceVariable
  ) where

import qualified Data.Bifunctor as Bifunctor
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Octizys.Classes.From (From (from))
import Octizys.Common.Id (ExpressionVariableId, TypeVariableId)
import Octizys.Common.LogicPath (LogicPath, addAtEnd)
import Octizys.Common.Name (Name)
import Octizys.Common.Qualifier (Qualifier)
import Octizys.Cst.Expression (Definition (name))
import Octizys.Cst.SourceInfo
  ( SourceInfo (span)
  , SourceVariable (name)
  )
import Octizys.Cst.Span (Span)
import Octizys.Cst.TopItem
  ( ImportModule
      ( ImportModuleAs'
      , ImportModuleUnqualified'
      , alias
      , items
      , path
      )
  , Module (definitions, imports)
  )
import Octizys.Effects.Accumulator.Interpreter (Accumulator)
import Octizys.Scope (ExpressionVariableInformation, ImportsScope)


data ImportError
  = MultipleDeclarationOf
      Name
      (Set (Definition SourceVariable SourceVariable))
      (Set SourceQualifier)
  deriving (Show)


newtype ImportErrors = ImportErrors' {unImportErrors :: [ImportError]}
  deriving (Show)
  deriving (Semigroup) via [ImportError]


data ImportWarning
  = ExpressionShadowing
      Name
      [Definition SourceVariable SourceVariable]
      [SourceQualifier]
  | -- | Multiple imports of the same symbol (based on their full qualifier).
    MultipleImports Name [SourceQualifier]
  | OverlappingQualifiers [SourceQualifier]
  deriving (Show, Eq, Ord)


data SourceQualifier = SourceQualifier' Qualifier Span
  deriving (Show, Eq, Ord)


{- | The objetive of this type is to facilitate the lookup of
symbols and paths.

Example:

import X(y,z);
import W qualified as T;
import R(z,j);

t :: Int;
t = 1;

Generates a resolution context like

@
  ResolutionContext {
   localSymbols = fromList [(t,{t :: Int = 1})]
   ,importedSymbols = fromList[
    (y,{X})
    ,(z,fromList [(X, source1),(R,source2)])
    ,(j,{R})
   ]
   ,qualifierSymbols = fromList[(T,W)]
   }
@
-}
data ResolutionContext = ResolutionContext'
  { localSymbols :: Map Name (Set (Definition SourceVariable SourceVariable))
  -- ^ Map unqualified symbols defined at the top level of
  -- the module to the set of fully qualified symbols with that name.
  , importedSymbols :: Map Name (Map Qualifier (Set Span))
  -- ^ Map unqualified imported symbols to the set of fully qualified
  -- symbols with that name.
  , qualifierSymbols :: Map LogicPath (Set SourceQualifier)
  -- ^ Map qualifier aliases to their full qualifiers.
  -- It utilizes a set since we allow overlapping qualifiers.
  }
  deriving (Show, Eq, Ord)


data ImportsContext e t = ImportsContext'
  { unqualifiedSymbols :: Map Name (Either (Definition e t) Qualifier)
  , qualifiedSymbols :: Map LogicPath (Set SourceQualifier)
  }


emptyResolutionContext :: ResolutionContext
emptyResolutionContext = ResolutionContext' mempty mempty mempty


{- | For imports like:

import W as T

@
  addQualifierSymbol T _ W
@
-}
addQualifierSymbol
  :: LogicPath -> Span -> LogicPath -> ResolutionContext -> ResolutionContext
addQualifierSymbol name sp value rc =
  rc
    { qualifierSymbols =
        Map.insertWith
          (<>)
          name
          (Set.singleton (SourceQualifier' (from value) sp))
          rc.qualifierSymbols
    }


{- | For imports like:

import X(y,z)

@
 addAloneSymbol y _ X _
 addAloneSymbol z _ X _
@
-}
addAloneSymbol
  :: Name -> Span -> LogicPath -> ResolutionContext -> ResolutionContext
addAloneSymbol name sp value rc =
  rc
    { importedSymbols =
        Map.insertWith
          Map.union
          name
          (Map.singleton (from value) (Set.singleton sp))
          rc.importedSymbols
    }


-- | Adds a CST.@ImportModule@ to the given resolution context.
addImportModule
  :: ResolutionContext
  -> ImportModule
  -> ResolutionContext
addImportModule rc x =
  case x of
    ImportModuleAs' {path = (info, path), alias} ->
      let asLogicPath = from path
       in case alias of
            Nothing ->
              addQualifierSymbol asLogicPath info.span asLogicPath rc
            Just alias2 ->
              addQualifierSymbol (from alias2) info.span asLogicPath rc
    ImportModuleUnqualified' {path = (_, path), items} ->
      foldl'
        (add (from path))
        rc
        (from @(NonEmpty (SourceInfo, Name)) items)
      where
        add
          :: LogicPath
          -> ResolutionContext
          -> (SourceInfo, Name)
          -> ResolutionContext
        add p rc2 (info, nam) =
          let qualifiedName = addAtEnd nam p
           in addAloneSymbol nam info.span qualifiedName rc2


-- | Adds a CST.@Definition@ to the given resolution context.
addDefinition
  :: ResolutionContext
  -> Definition SourceVariable SourceVariable
  -> ResolutionContext
addDefinition rc def =
  -- Note: we ignore the case where there is a qualifier inside
  -- for the name as this is should be the identifier of a definition.
  let sv = snd def.name
   in rc
        { localSymbols =
            Map.insertWith
              (<>)
              sv.name
              (Set.singleton def)
              rc.localSymbols
        }


{- | Translates a list of import declarations to a
mapping of symbols and qualifiers to the possible real qualifiers
to lookup.
-}
makeResolutionContext
  :: Module SourceVariable SourceVariable
  -> ResolutionContext
makeResolutionContext modu =
  let imports = fst <$> modu.imports
      definitions = fst <$> modu.definitions
      rcImportsAdded = foldl' addImportModule emptyResolutionContext imports
   in foldl' addDefinition rcImportsAdded definitions


lookupExpressionSourceVariable
  :: ImportsScope
  -> ExpressionVariableId
  -> ImportsContext ExpressionVariableId TypeVariableId
  -> Either () ExpressionVariableInformation
lookupExpressionSourceVariable _importsScope _resolutionContext _svar = undefined


{- | Find the unqualified symbols that are defined locally and imported at the
same time.
-}
filterImportedAndLocal
  :: ResolutionContext
  -> Either
      ImportErrors
      ()
filterImportedAndLocal rc =
  let
    localAndImported =
      Map.intersectionWith
        (\x y -> (x, y))
        rc.localSymbols
        rc.importedSymbols
    g qual spans acc = Set.map (SourceQualifier' qual) spans <> acc
    f (name, (locals, importeds)) =
      MultipleDeclarationOf
        name
        locals
        (Map.foldrWithKey g mempty importeds)
    errors = f <$> Map.toList localAndImported
   in
    case errors of
      [] -> pure ()
      _ -> Left (ImportErrors' {unImportErrors = errors})


filterLocalDuplicates
  :: Map Name (Set (Definition SourceVariable SourceVariable))
  -> Either
      ImportErrors
      (Map Name (Either (Definition SourceVariable SourceVariable) Qualifier))
filterLocalDuplicates m =
  let
    (mapWithDuplicates, uniquesMap) = Map.partition (\x -> length x > 1) m
    duplicates :: [ImportError] =
      (\(x, y) -> MultipleDeclarationOf x y mempty)
        <$> Map.toList mapWithDuplicates
   in
    case duplicates of
      [] ->
        pure $
          Map.mapMaybe
            ( \s ->
                case Set.toList s of
                  [] -> Nothing
                  (x : _) -> Just (Left x)
            )
            uniquesMap
      _ -> Left $ ImportErrors' duplicates


filterUnQualifiedImportDuplicates
  :: Map Name (Map Qualifier (Set Span))
  -> Either
      ImportErrors
      (Map Name (Either (Definition SourceVariable SourceVariable) Qualifier))
filterUnQualifiedImportDuplicates m =
  let
    (mapWithDuplicates, uniquesMap) = Map.partition (\x -> length x > 1) m
    makeSourceQualifierSet :: (Qualifier, Set Span) -> Set SourceQualifier
    makeSourceQualifierSet (qual, spans) = Set.map (SourceQualifier' qual) spans

    duplicates :: [ImportError] =
      ( \(x, y) ->
          MultipleDeclarationOf
            x
            mempty
            ( foldl
                (<>)
                mempty
                (makeSourceQualifierSet <$> Map.toList y)
            )
      )
        <$> Map.toList mapWithDuplicates
   in
    case duplicates of
      [] ->
        pure $
          Map.mapMaybe
            ( \ma ->
                case Map.toList ma of
                  [] -> Nothing
                  ((qual, _) : _) -> Just (Right qual)
            )
            uniquesMap
      _ -> Left $ ImportErrors' duplicates


simplifyUnqualifiedSymbols
  :: Error ImportErrors :> e
  => Accumulator ImportWarning :> e
  => ResolutionContext
  -> Eff
      e
      (Map Name (Either (Definition SourceVariable SourceVariable) Qualifier))
simplifyUnqualifiedSymbols rc =
  let
    results =
      Bifunctor.second (const mempty) (filterImportedAndLocal rc)
        <> filterLocalDuplicates rc.localSymbols
        <> filterUnQualifiedImportDuplicates rc.importedSymbols
   in
    case results of
      Left e -> throwError e
      Right m -> pure m


makeImportsContext
  :: Error ImportErrors :> e
  => Accumulator ImportWarning :> e
  => Module SourceVariable SourceVariable
  -> Eff e (ImportsContext SourceVariable SourceVariable)
makeImportsContext modul = do
  let rc = makeResolutionContext modul
  maybeUnqualifiedSymbols <-
    runErrorNoCallStack (simplifyUnqualifiedSymbols rc)
  case maybeUnqualifiedSymbols of
    Left errors1 ->
      throwError errors1
    Right unqualifiedSymbols ->
      pure
        ImportsContext'
          { unqualifiedSymbols
          , qualifiedSymbols =
              rc.qualifierSymbols
          }
