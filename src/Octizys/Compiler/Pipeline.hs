{-# LANGUAGE DataKinds #-}

module Octizys.Compiler.Pipeline where

import Data.Aeson (ToJSON (toJSON))
import Data.Bifunctor (Bifunctor (..))
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import EffectfulParserCombinators.Interpreter (runFullParser)
import Octizys.Classes.From (From (from))
import Octizys.Common.Format.Config (defaultConfiguration)
import qualified Octizys.Compiler.Error as Compiler
import qualified Octizys.Compiler.Module.Build as Module
import qualified Octizys.Compiler.Module.Index as Module
import qualified Octizys.Compiler.Module.Index as Module.Index
import Octizys.Compiler.Package.Build (makeEmptyDependencyTree)
import qualified Octizys.Compiler.Package.Build as Package
import qualified Octizys.Compiler.Package.Index as Package
import qualified Octizys.Compiler.Package.Index as Package.Index
import Octizys.Compiler.Package.Reference (Reference)
import qualified Octizys.Compiler.Stage as Compiler.Stage
import qualified Octizys.Compiler.Warn as Compiler
import Octizys.Effects.Accumulator.Effect (Accumulator)
import Octizys.Effects.Accumulator.Utils
  ( accumulateErrors
  , throwAccumulator
  )
import Octizys.Effects.FileReader.Effect (FileReader)
import qualified Octizys.Effects.FileReader.Effect as FileReader
import qualified Octizys.FrontEnd.Format.TopItem as Cst
import qualified Octizys.FrontEnd.Parser.TopItem as Parser
import Octizys.Logging.Effect (Log)
import Octizys.Logging.Entry (field, fieldWith)
import qualified Octizys.Logging.Loggers as Log


fromRightOrThrow
  :: Error b :> e
  => Eff e (Either b a)
  -> Eff e a
fromRightOrThrow action = do
  maybeValue <- action
  case maybeValue of
    Left e -> throwError e
    Right x -> pure x


parseModule
  :: Error Compiler.Error :> e
  => FileReader :> e
  => Log :> e
  => Module.Path
  -> Eff e (Module.BuildState Compiler.Stage.Parsed)
parseModule path = do
  sourceCode <-
    fromRightOrThrow
      (castFileError $ FileReader.readFile filePath)
  out <-
    fromRightOrThrow (runParser sourceCode)
  Log.trace
    "Parsing result"
    [ field "logic path" logicPath
    , field "system path" filePath
    , fieldWith
        toJSON
        (Cst.formatModule defaultConfiguration)
        "parsed module"
        out
    ]
  pure (Module.Parsed out)
  where
    filePath = Module.getFilePath path
    logicPath = Module.getLogicPath path
    runParser code =
      castParserError
        code
        (runFullParser code (Parser.parseModule filePath logicPath))
    castParserError code =
      (first (Compiler.ParserError filePath logicPath code) <$>)
    castFileError = (first from <$>)


parseModules
  :: Accumulator Compiler.Error :> e
  => FileReader :> e
  => Log :> e
  => [Module.Path]
  -> Reference
  -> Eff e (Package.BuildState Compiler.Stage.Parsed)
parseModules modulePaths packageRef = do
  _ <- accumulateErrors parseModule modulePaths
  let
    out =
      Package.makeBuildState
        Module.Index.empty
        makeEmptyDependencyTree
        packageRef
        Package.Index.empty
  Log.trace
    "Package modules parsed"
    [ field "files" modulePaths
    , fieldWith
        Package.toJSONBuildState
        Package.prettyBuildState
        "parsed modules"
        out
    ]
  pure out


resolveModuleSymbols
  :: Accumulator Compiler.Error :> e
  => Accumulator Compiler.Warn :> e
  => Module.BuildState Compiler.Stage.Parsed
  -> Eff e (Module.BuildState Compiler.Stage.SymbolsSolved)
resolveModuleSymbols = undefined


resolveSymbols
  :: Accumulator Compiler.Error :> e
  => Accumulator Compiler.Warn :> e
  => Package.BuildState Compiler.Stage.Parsed
  -> Eff e (Package.BuildState Compiler.Stage.SymbolsSolved)
resolveSymbols = undefined


inferModuleTypes
  :: Accumulator Compiler.Error :> e
  => Accumulator Compiler.Warn :> e
  => Log :> e
  => Module.BuildState Compiler.Stage.SymbolsSolved
  -> Eff e (Module.BuildState Compiler.Stage.TypesChecked)
inferModuleTypes = undefined


inferTypes
  :: Accumulator Compiler.Error :> e
  => Accumulator Compiler.Warn :> e
  => Log :> e
  => Package.BuildState Compiler.Stage.SymbolsSolved
  -> Eff e (Package.BuildState Compiler.Stage.TypesChecked)
inferTypes = undefined


optimizeModule
  :: Error Compiler.Error :> e
  => Accumulator Compiler.Warn :> e
  => Log :> e
  => Module.BuildState Compiler.Stage.TypesChecked
  -> Eff e (Module.BuildState Compiler.Stage.Optimized)
optimizeModule = undefined


optimize
  :: Accumulator Compiler.Error :> e
  => Accumulator Compiler.Warn :> e
  => Log :> e
  => Package.BuildState Compiler.Stage.TypesChecked
  -> Eff e (Package.BuildState Compiler.Stage.Optimized)
optimize = undefined


compilePackage
  :: Error [Compiler.Error] :> e
  => Accumulator Compiler.Warn :> e
  => FileReader :> e
  => Log :> e
  => FilePath
  -> Reference
  -> Eff e (Package.BuildState Compiler.Stage.Optimized)
compilePackage fileOrFolder packageRef = do
  index <- Package.build fileOrFolder
  let
    modulePaths = Package.listModules index
  parsed <- throwErrors $ parseModules modulePaths packageRef
  solved <- throwErrors $ resolveSymbols parsed
  typed <- throwErrors $ inferTypes solved
  throwErrors $ optimize typed
  where
    throwErrors = throwAccumulator @Compiler.Error
