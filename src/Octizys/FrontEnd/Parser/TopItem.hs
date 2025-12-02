module Octizys.FrontEnd.Parser.TopItem where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Effectful (Eff, (:>))
import EffectfulParserCombinators.Combinators
  ( errorCustom
  , hidden
  , many
  , optional
  , (<?>)
  , (<|>)
  )
import EffectfulParserCombinators.Effect (Parser)
import Octizys.Common.LogicPath (addAtEnd)
import Octizys.FrontEnd.Cst.Expression (Definition)
import Octizys.FrontEnd.Cst.SourceInfo
  ( SourceInfo
  , SourceVariable
  , makeSourceInfo
  , name
  , qualifier
  )
import Octizys.FrontEnd.Cst.TopItem
  ( ImportAlias (ImportAlias', path, _as)
  , ImportItem (ImportVariable, info, name)
  , ImportItems (ImportItems', items, lastComma, lastItem)
  , ImportModule
    ( ImportModuleAs'
    , ImportModuleUnqualified'
    , alias
    , items
    , lparen
    , path
    , rparen
    , unqualified
    , _import
    )
  , Module (Module', definitions, imports, lastComments)
  , ModulePath (ModuleLogicPath, ModuleVarPath)
  , TopItem
    ( TopDefinition
    , TopImport
    , definition
    , importModule
    , semicolon
    )
  )
import Octizys.FrontEnd.Parser.Common
  ( OctizysParseError (EmptyImportList)
  , asKeyword
  , comments
  , importKeyword
  , leftParen
  , localVariable
  , rightParen
  , sourceVariableParser
  , trailingList
  , unqualifiedKeyword
  )
import qualified Octizys.FrontEnd.Parser.Common as Common
import Octizys.FrontEnd.Parser.Expression (definitionParser)


modulePathParser
  :: Parser OctizysParseError :> es
  => Eff es (SourceInfo, ModulePath)
modulePathParser = do
  (var, varInfo) <- sourceVariableParser
  let path =
        case var.qualifier of
          Just p -> ModuleLogicPath (addAtEnd var.name p)
          Nothing -> ModuleVarPath var.name
  pure (varInfo, path)


importItemParser
  :: Parser OctizysParseError :> es
  => Eff es ImportItem
importItemParser = do
  (var, info) <- localVariable
  pure ImportVariable {info, name = var.name}


-- | This can raise an error
importItemsParser
  :: Parser OctizysParseError :> es
  => Eff es ImportItems
importItemsParser = do
  (items, maybeLast) <- trailingList Common.comma importItemParser
  case maybeLast of
    Just (lastItem, lastComma) -> pure $ ImportItems' {items, lastItem, lastComma}
    Nothing -> errorCustom EmptyImportList


importAliasParser
  :: Parser OctizysParseError :> es
  => Eff es ImportAlias
importAliasParser = do
  asInfo <- asKeyword
  modulePath <- modulePathParser <?> ('l' :| "ogic path")
  pure
    ImportAlias'
      { _as = asInfo
      , path = modulePath
      }


importModuleParser
  :: Parser OctizysParseError :> es
  => Eff es ImportModule
importModuleParser = do
  _import <- importKeyword
  maybeUnqualified <- optional unqualifiedKeyword
  path <- modulePathParser
  case maybeUnqualified of
    Nothing -> do
      alias <- optional importAliasParser
      pure ImportModuleAs' {_import, path, alias}
    Just unqualified -> do
      lparen <- leftParen
      items <- importItemsParser
      rparen <- rightParen
      pure
        ImportModuleUnqualified'
          { _import
          , unqualified
          , path
          , lparen
          , items
          , rparen
          }


parseTopItem
  :: Parser OctizysParseError :> es
  => Eff es TopItem
parseTopItem = do
  item <-
    (TopDefinition <$> definitionParser)
      <|> (TopImport <$> importModuleParser)
  item <$> Common.semicolon


parseModule
  :: Parser OctizysParseError :> es
  => Eff es (Module SourceVariable SourceVariable)
parseModule = do
  items <- many parseTopItem
  let (definitions, imports) = splitItems items ([], [])
  lastCommentsRaw <- hidden comments
  case lastCommentsRaw of
    [] ->
      pure
        Module'
          { lastComments = Nothing
          , definitions
          , imports
          }
    _ ->
      let lastCommentsJust =
            makeSourceInfo
              undefined
              lastCommentsRaw
              Nothing
       in pure
            Module'
              { lastComments = Just lastCommentsJust
              , definitions
              , imports
              }
  where
    splitItems
      :: [TopItem]
      -> ( [(Definition SourceVariable SourceVariable, SourceInfo)]
         , [(ImportModule, SourceInfo)]
         )
      -> ( [(Definition SourceVariable SourceVariable, SourceInfo)]
         , [(ImportModule, SourceInfo)]
         )
    splitItems [] (defs, imports) = (reverse defs, reverse imports)
    splitItems (item : remain) (defs, imports) =
      case item of
        TopDefinition {definition, semicolon} ->
          splitItems remain ((definition, semicolon) : defs, imports)
        TopImport {importModule, semicolon} ->
          splitItems remain (defs, (importModule, semicolon) : imports)


parserDefinitions
  :: Parser OctizysParseError :> es
  => Eff es [Definition SourceVariable SourceVariable]
parserDefinitions = many definitionParser
