{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module Octizys.Compiler.Compiler
  ( compile
  , CompilerConfig (CompilerConfig')
  , RootPaths (RootPaths')
  ) where

import Control.Arrow ((<<<))
import Control.Monad (forM_, unless)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Text (Text)
import qualified Data.Text as Text
import Effectful (Eff, runEff)
import Effectful.Error.Static
  ( Error
  , runError
  , runErrorNoCallStack
  , throwError
  )
import Effectful.Internal.Effect ((:>))
import Effectful.Reader.Static (Reader, runReader)
import EffectfulParserCombinators.Interpreter
  ( runFullParser
  )
import Prettyprinter (Doc)
import Prettyprinter.Render.Text (renderStrict)

-- import Octizys.Compiler.Format
--   ( buildFormatContext
--   , buildInferenceErrorReport
--   , formatE
--   )
import Octizys.Effects.Console.Interpreter (runConsole)
import Octizys.Effects.Logger.ConsoleInterpreter (runLog)
import Octizys.Effects.Logger.Effect (LogLevel, Logger)

-- import Octizys.FrontEnd.Cst.Expression (ExpressionVariableId)
import Octizys.FrontEnd.Cst.TopItem (Module)

-- import Octizys.Inference.ConstraintsGeneration (InferenceState)
-- import Octizys.Inference.ConstraintsSolver (solveDefinitionsType)

import Octizys.FrontEnd.Parser.TopItem (parseModule)

-- import Octizys.Pretty.FormatContext (Configuration)
-- import Octizys.Pretty.Formatter (format)

import Data.Bifunctor (Bifunctor (first))
import Data.Either (partitionEithers)
import Data.Functor (($>))
import EffectfulParserCombinators.Error (ParserError, humanReadableError)
import Octizys.Ast.Type (TypeVariable)
import Octizys.Classes.From (From (from))
import Octizys.Common.Id (ExpressionVariableId, TypeVariableId)
import Octizys.Common.LogicPath (LogicPath)
import qualified Octizys.Common.LogicPath as LogicPath
import Octizys.Common.Name (makeName)
import Octizys.Effects.Accumulator.Effect (Accumulator, accumulate)
import Octizys.Effects.Accumulator.Interpreter (runAccumulatorFull)
import Octizys.Effects.FileReader.Effect (FileReadError, FileReader)
import qualified Octizys.Effects.FileReader.Effect as FileReader
import Octizys.Effects.FileReader.Interpreter (runFileReader)
import Octizys.FrontEnd.Cst.SourceInfo (SourceVariable)
import Octizys.FrontEnd.Parser.Common (OctizysParseError)
import Octizys.StaticLog (logError, logInfo, logTrace, logWarn)
import Prettyprinter (Pretty (pretty), defaultLayoutOptions, layoutPretty)
import qualified Prettyprinter as Pretty


-- TODO:STUB
data PathIndexError = PathIndexError'
  deriving (Show, Eq, Ord)


-- TODO:STUB

{- | This storages the needed information to exchange between
logic paths and system paths
-}
data PathIndex = PathIndex'
  deriving (Show, Eq, Ord)


-- TODO:STUB
lookupLogicPath
  :: Error PathIndexError :> e
  => PathIndex
  -> LogicPath
  -> Eff e FilePath
lookupLogicPath = undefined


render :: Doc ann -> Text
render =
  renderStrict
    <<< layoutPretty defaultLayoutOptions


-- TODO:STUB
lookupSystemPath
  :: Error PathIndexError :> e
  => Logger :> e
  => PathIndex
  -> FilePath
  -> Eff e LogicPath
lookupSystemPath _ systemPath = do
  $(logTrace) (Text.pack ("Search of logic path for: " <> systemPath))
  let maybeLogicPath = LogicPath.singleton <$> makeName "StubName"
  case maybeLogicPath of
    Just logicPath -> do
      $(logTrace)
        ( render
            ( pretty ("Found logic path for: " <> systemPath <> ", is ")
                <> pretty logicPath
            )
        )
      $(logTrace)
        (render (pretty ("Search of logic path for:" <> systemPath <> ", ends")))
      pure logicPath
    Nothing -> do
      $(logError)
        ( render
            (pretty ("No logic path found for : " <> systemPath))
        )
      $(logTrace)
        ( render
            (pretty ("Search of logic path for:" <> systemPath <> ", ends"))
        )
      throwError
        PathIndexError'


-- TODO:STUB
data LogicPathError
  = LogicPathNotFound FilePath
  deriving (Show, Eq)


instance Pretty LogicPathError where
  pretty (LogicPathNotFound path) =
    pretty @Text
      "Error, file is not part of the project or it's dependences, file:"
      <> Pretty.hardline
      <> Pretty.nest 4 (pretty path)
      <> Pretty.hardline


-- <> Pretty.nest 4 "lookup done on files:"
-- <> Pretty.nest 4 (pretty roots)

data OctizysError
  = OctizysParserError FilePath Text (ParserError OctizysParseError)
  | OctizysFileReadError FileReadError
  | OctizysLogicPathError LogicPathError
  | OctizysPathIndexError PathIndexError
  deriving (Show, Eq)


instance From OctizysError FileReadError where
  from = OctizysFileReadError


instance From OctizysError LogicPathError where
  from = OctizysLogicPathError


-- TODO:STUB
instance Pretty OctizysError where
  pretty (OctizysParserError _ src parseError) =
    humanReadableError Nothing src parseError
  pretty (OctizysFileReadError e) = pretty e
  pretty (OctizysLogicPathError e) = pretty e


-- TODO:STUB
data OctizysWarn = OctizysWarn
  deriving (Show, Eq, Ord)


-- TODO:STUB
data CompilerConfig = CompilerConfig'
  deriving (Show, Eq, Ord)


-- TODO:STUB
data DependencyTree = DependencyTree
  { rootPaths :: RootPaths
  , pathIndex :: PathIndex
  }
  deriving (Show, Eq, Ord)


-- TODO:STUB
makeEmptyDependencyTree :: RootPaths -> PathIndex -> DependencyTree
makeEmptyDependencyTree = undefined


-- TODO:STUB
data AstModule a = AstModule


-- TODO:STUB
data SymbolResolutionEnvironment = SymbolResolutionEnvironment


-- TODO:STUB
data InferredTypesEnvironment = InferredTypesEnvironment


-- TODO:STUB
newtype RootPaths = RootPaths' {unRootPaths :: [FilePath]}
  deriving (Show, Eq, Ord)


-- TODO:STUB
instance Pretty RootPaths where
  pretty (RootPaths' rs) = pretty rs


fromRightOrThrow
  :: Error b :> e
  => Eff e (Either b a)
  -> Eff e a
fromRightOrThrow action = do
  maybeValue <- action
  case maybeValue of
    Left e -> throwError e
    Right x -> pure x


parseFile
  :: Reader CompilerConfig :> e
  => Accumulator OctizysWarn :> e
  => Error OctizysError :> e
  => FileReader :> e
  => Logger :> e
  => FilePath
  -> LogicPath
  -> Eff e (Module SourceVariable SourceVariable)
parseFile spath lpath = do
  $(logTrace) (render (pretty ("Starting file parser IO: " <> spath)))
  $(logTrace) (render (pretty ("Obtaining file: " <> spath)))
  sourceCode <-
    fromRightOrThrow (first (from @OctizysError) <$> FileReader.readFile spath)
  $(logTrace) (render (pretty ("File obtained: " <> spath)))
  $(logTrace) (render (pretty ("Parsing file: " <> spath)))
  out <-
    fromRightOrThrow
      ( first (OctizysParserError spath sourceCode)
          <$> runFullParser sourceCode (parseModule spath lpath)
      )
  $(logTrace) (render (pretty ("File parsed: " <> spath)))
  $(logTrace) (render (pretty ("Finish file parser IO: " <> spath)))
  pure out


-- TODO:STUB
buildPathIndex
  :: Accumulator OctizysWarn :> e
  => Logger :> e
  => RootPaths
  -> Eff e PathIndex
buildPathIndex rootPaths = do
  $(logInfo) (render (pretty @Text "Build of dependency tree start"))
  $(logTrace)
    ( render
        (pretty @Text "RootPaths to build dependency tree:" <> pretty rootPaths)
    )
  let tree = PathIndex'
  $(logInfo) (render (pretty @Text "Build of dependency tree ends"))
  pure tree


annotateInput
  :: Functor m
  => (x -> m a)
  -> (x -> m (x, a))
annotateInput f x = (\w -> (x, w)) <$> f x


-- TODO:STUB
parseAndMakeDependencyTree
  :: Reader CompilerConfig :> e
  => Accumulator OctizysWarn :> e
  => Accumulator OctizysError :> e
  => FileReader :> e
  => Logger :> e
  => [FilePath]
  -> RootPaths
  -> PathIndex
  -> Eff e (DependencyTree, [Module SourceVariable SourceVariable])
parseAndMakeDependencyTree pathsToFilesToCompile rootPaths pathIndex = do
  $(logInfo)
    ( render
        (pretty @Text "Start parsing an making dependency tree based on index")
    )
  result <-
    loop pathsToFilesToCompile [] (makeEmptyDependencyTree rootPaths pathIndex)

  $(logTrace)
    (render (pretty @Text "Finished parsing and making dependency tree"))
  pure result
  where
    loop [] modules tree = pure (tree, modules)
    loop remainPaths modulesAcc treeAcc = do
      newModules <- parseStep remainPaths
      let (newTreeAcc, newPaths) = dependencyStep newModules treeAcc
      loop newPaths (modulesAcc ++ newModules) newTreeAcc

    dependencyStep
      :: [Module SourceVariable SourceVariable]
      -> DependencyTree
      -> (DependencyTree, [FilePath])
    dependencyStep _ d = (d, [])

    parseStep paths = do
      maybeLogicAndSystemPaths
        :: [(FilePath, Either PathIndexError LogicPath)] <-
        mapM
          ( annotateInput
              (runErrorNoCallStack <<< lookupSystemPath pathIndex)
          )
          paths
      let (indexErrors, logicAndSystemPaths)
            :: ([(FilePath, PathIndexError)], [(FilePath, LogicPath)]) = partition maybeLogicAndSystemPaths
      forM_
        ((\(_, x) -> OctizysPathIndexError x) <$> indexErrors)
        (accumulate @OctizysError)

      maybeCSTs <-
        mapM
          (runErrorNoCallStack <<< uncurry parseFile)
          logicAndSystemPaths
      let (parsingErrors, csts) = partitionEithers maybeCSTs
      forM_
        parsingErrors
        (accumulate @OctizysError)
      pure csts

    partition :: [(p, Either a b)] -> ([(p, a)], [(p, b)])
    partition v = partitionAux v [] []

    partitionAux [] acc1 acc2 = (acc1, acc2)
    partitionAux ((p, e) : remain) acc1 acc2 =
      case e of
        Left err -> partitionAux remain ((p, err) : acc1) acc2
        Right x -> partitionAux remain acc1 ((p, x) : acc2)


solveSymbols
  :: Reader CompilerConfig :> e
  => Accumulator OctizysWarn :> e
  => Accumulator OctizysError :> e
  => Logger :> e
  => DependencyTree
  -> [Module SourceVariable SourceVariable]
  -> Eff
      e
      (SymbolResolutionEnvironment, [Module ExpressionVariableId TypeVariableId])
solveSymbols _ _ = do
  $(logInfo) (render (pretty @Text "Solving symbols"))
  pure (SymbolResolutionEnvironment, [])


typeCheckAndInference
  :: Reader CompilerConfig :> e
  => Accumulator OctizysWarn :> e
  => Accumulator OctizysError :> e
  => Logger :> e
  => SymbolResolutionEnvironment
  -> [Module ExpressionVariableId TypeVariableId]
  -> Eff
      e
      (InferredTypesEnvironment, [AstModule TypeVariable])
typeCheckAndInference _ _ = do
  $(logInfo) (render (pretty @Text "Inferring and checking types"))
  pure (InferredTypesEnvironment, [])


generateCode
  :: Reader CompilerConfig :> e
  => Logger :> e
  => InferredTypesEnvironment
  -> [AstModule TypeVariable]
  -> Eff e ((), ())
generateCode _ _ = do
  $(logInfo) (render (pretty @Text "Generating Code"))
  pure ((), ())


logErrors
  :: Logger :> e
  => [OctizysError]
  -> Eff e ()
logErrors errors =
  $(logError) (render (pretty errors))


logWarns
  :: Logger :> e
  => [OctizysWarn]
  -> Eff e ()
logWarns _ = $(logWarn) (render (pretty @Text "Logging Warns!"))


compileEffectful
  :: Reader CompilerConfig :> e
  => Accumulator OctizysWarn :> e
  => Error [OctizysError] :> e
  => FileReader :> e
  => Logger :> e
  => NonEmpty FilePath
  -> RootPaths
  -> Eff e ()
compileEffectful paths rootPaths = do
  pathIndex <- buildPathIndex rootPaths
  throwOrAdvance
    (parseAndMakeDependencyTree (toList paths) rootPaths pathIndex)
    (merge solveSymbols (merge typeCheckAndInference generateCode))
    $> ()
  where
    merge f1 f2 mod1 env1 = throwOrAdvance (f1 mod1 env1) f2

    throwOrAdvance currentStep nextStep = do
      ((environment, modules), errors) <-
        runAccumulatorFull @OctizysError [] currentStep
      unless (null errors) (throwError errors)
      nextStep environment modules


--  throwOrAdvance
--    generateCode
--    ( throwOrAdvance
--        typeCheckAndInference
--        (throwOrAdvance solveSymbols $ parseAndMakeDependencyTree (toList paths))
--    )
--  where
--    throwOrAdvance nextStep currentStep = do
--      ((modules, environment), errors) <-
--        runAccumulatorFull @OctizysError [] currentStep
--      unless (null errors) (throwError errors)
--      nextStep environment modules

compile :: NonEmpty FilePath -> LogLevel -> RootPaths -> IO ()
compile paths level rootPaths =
  runEff
    ( runFileReader
        ( runConsole
            ( runLog
                level
                ( runErrorsAndWarns (compileEffectful paths rootPaths)
                )
            )
        )
    )
  where
    runErrorsAndWarns eff = do
      (_, warns) <-
        runAccumulatorFull
          []
          ( do
              maybeNoErrors <- runError @[OctizysError] (runReader CompilerConfig' eff)
              case maybeNoErrors of
                Left (_, errors) -> logErrors errors
                Right _ -> pure ()
          )
      unless (null warns) (logWarns warns)

-- render :: forall a ann. (a -> Doc ann) -> a -> Text
-- render prettifier =
--  Prettyprinter.Render.Text.renderStrict
--    <<< Prettyprinter.layoutPretty Prettyprinter.defaultLayoutOptions
--    <<< prettifier
--
-- reportErrorWith
--  :: forall e es ann
--   . Console :> es
--  => (e -> Doc ann)
--  -> Eff (Error e : es) ()
--  -> Eff es ()
-- reportErrorWith prettier =
--  runErrorNoCallStackWith
--    (putLine <<< render prettier )
--
-- reportErrorShow
--  :: forall e es
--   . (Console :> es, Show e)
--  => Eff (Error e : es) ()
--  -> Eff es ()
-- reportErrorShow = reportErrorWith (pretty <<< ppShow)
--
--
-- reportErrorPretty
--  :: forall e es
--   . (Console :> es, Pretty e)
--  => Eff (Error e : es) ()
--  -> Eff es ()
-- reportErrorPretty = reportErrorWith pretty
--
--
--

-- readSymbolResolution
--  :: SymbolResolution :> es
--  => Eff (Reader SymbolResolutionState : es) a
--  -> Eff es a
-- readSymbolResolution action = do
--  st <- getSymbolResolutionState
--  runReader st action
--
--
-- loadFile :: FilePath -> IO Text
-- loadFile = Text.readFile
--
--
-- parseFile
--  :: SymbolResolution :> es
--  => Configuration
--  -> FilePath
--  -> Text
--  -> Eff es (Either (Doc ann) Module)
-- parseFile config name content = do
--  maybeModule <- run parseModule
--  case maybeModule of
--    Left e -> do
--      ctx <- readSymbolResolution $ buildFormatContext config
--      pure $
--        Left $
--          format
--            ctx
--            ( makeParseErrorReport
--                ctx
--                (NonEmpty.nonEmpty name)
--                content
--                e
--            )
--    Right (m, _) -> pure $ Right m
--  where
--    startState = makeInitialState content
--    run = runErrorNoCallStack <<< runParserWith startState

-- compileFile
--   :: (SymbolResolution :> es,
--     State InferenceState :> es,
--     IOE :> es)
--   => Configuration
--   -> FilePath
--   -> Eff es (Either (Doc ann) [Ast.Definition])
-- compileFile config path = do
--   content <- liftIO $ loadFile path
--   mod <- parseFile config path content
--   maybeDefs <- runErrorNoCallStack (solveDefinitionsType mod.definitions)
--   case maybeDefs of
--     Left e -> formatE config buildInferenceErrorReport e

--  mapM_ (run <<< void <<< compileFile) paths
--  where
--    run =
--          runEff
--          <<< runConsole
--          <<< runLog logLevel
--          <<< void
--          <<< runState @DefinedSymbols mempty
--          <<< runState Inference.initialInferenceState
--          <<< runState initialSymbolResolutionState
--          <<< reportErrorShow @SymbolResolutionError
--          <<< reportErrorPretty @EvaluationError
--          <<< runSymbolResolution

-- newtype DefinedSymbols = DefinedSymbols'
--
--  deriving
--    ( Show
--    , Eq
--    , Ord
--    , Semigroup
--    , Monoid
--    )
--    via (Map ExpressionVariableId Ast.Expression)
--
--
-- compileFile
--  :: ( Console :> es
--     , SymbolResolution :> es
--     , State Inference.InferenceState :> es
--     , Error EvaluationError :> es
--     , State DefinedSymbols :> es
--     , Logger :> es
--     )
--  => FilePath
--  -> Eff es Ast.Type
-- compileFile = undefined
