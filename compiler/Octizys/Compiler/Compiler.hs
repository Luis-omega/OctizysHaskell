{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module Octizys.Compiler.Compiler
  ( compile
  , CompilerConfig (CompilerConfig')
  ) where

import Control.Arrow ((<<<))
import Control.Monad (forM_, unless)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Text (Text)
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
import Octizys.Logging.Effect (Log)
import Octizys.Logging.Interpreters.Console (runLog)
import Octizys.Logging.Levels (Level)

-- import Octizys.FrontEnd.Cst.Expression (ExpressionVariableId)
import Octizys.FrontEnd.Cst.TopItem (Module (systemPath))

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
import Octizys.Effects.Accumulator.Effect (Accumulator, accumulate)
import Octizys.Effects.Accumulator.Interpreter (runAccumulatorFull)
import Octizys.Effects.FileReader.Effect (FileReadError, FileReader)
import qualified Octizys.Effects.FileReader.Effect as FileReader
import Octizys.Effects.FileReader.Interpreter (runFileReader)
import Octizys.FrontEnd.Cst.SourceInfo (SourceVariable)
import Octizys.FrontEnd.Parser.Common (OctizysParseError)
import Prettyprinter (Pretty (pretty), defaultLayoutOptions, layoutPretty)
import qualified Prettyprinter as Pretty

import Octizys.Logging.Entry (field, fieldWith)
import qualified Octizys.Logging.Loggers as Log
import Octizys.PathResolution.DependencyTree
  ( DependencyTree
  , makeEmptyDependencyTree
  )
import Octizys.PathResolution.PathIndex
  ( PathIndex
  , PathIndexError
  , RootPaths
  , lookupSystemPath
  , makePathIndex
  )
import Octizys.Pretty.FormatContext (defaultFormatContext)
import Octizys.Pretty.Formatter (Formatter (format))

import Data.Aeson (ToJSON (toJSON))
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import GHC.Generics (Generic, Generically (..))


render :: Doc ann -> Text
render =
  renderStrict
    <<< layoutPretty defaultLayoutOptions


data OctizysError
  = OctizysParserError FilePath Text (ParserError OctizysParseError)
  | OctizysFileReadError FileReadError
  | OctizysPathIndexError PathIndexError
  deriving (Show, Eq, Generic)
  deriving (ToJSON) via Generically OctizysError


instance From OctizysError FileReadError where
  from = OctizysFileReadError


-- TODO:STUB
instance Pretty OctizysError where
  pretty (OctizysParserError _ src parseError) =
    humanReadableError Nothing src parseError
  pretty (OctizysFileReadError e) = pretty e
  pretty (OctizysPathIndexError e) = pretty e


-- TODO:STUB
data OctizysWarn = OctizysWarn
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically OctizysWarn


instance Pretty OctizysWarn where
  pretty x = pretty $ show x


-- TODO:STUB
data CompilerConfig = CompilerConfig'
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically CompilerConfig


-- TODO:STUB
data AstModule a = AstModule
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (AstModule a)


-- TODO:STUB
data SymbolResolutionEnvironment = SymbolResolutionEnvironment
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically SymbolResolutionEnvironment


-- TODO:STUB
data InferredTypesEnvironment = InferredTypesEnvironment
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically InferredTypesEnvironment


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
  => Log :> e
  => FilePath
  -> LogicPath
  -> Eff e (Module SourceVariable SourceVariable)
parseFile spath lpath = do
  Log.trace
    "Starting file parser IO"
    [ field "logic path" lpath
    , field "system path" spath
    ]
  Log.trace
    "Obtaining file"
    [ field "logic path" lpath
    , field "system path" spath
    ]
  sourceCode <-
    fromRightOrThrow (first (from @OctizysError) <$> FileReader.readFile spath)
  Log.trace
    "File obtained"
    [ field "logic path" lpath
    , field "system path" spath
    ]
  Log.trace
    "Parsing file"
    [ field "logic path" lpath
    , field "system path" spath
    ]
  out <-
    fromRightOrThrow
      ( first (OctizysParserError spath sourceCode)
          <$> runFullParser sourceCode (parseModule spath lpath)
      )
  Log.trace
    "File parsed"
    [ field "logic path" lpath
    , field "system path" spath
    ]
  Log.trace
    "Finish file parser IO"
    [ field "logic path" lpath
    , field "system path" spath
    ]
  pure out


-- TODO:STUB
buildPathIndex
  :: Accumulator OctizysWarn :> e
  => Log :> e
  => RootPaths
  -> Eff e PathIndex
buildPathIndex rootPaths = do
  Log.info "Build of dependency tree start" [field "root paths" rootPaths]
  let tree = makePathIndex rootPaths
  Log.info "Build of dependency tree end" [field "root paths" rootPaths]
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
  => Log :> e
  => [FilePath]
  -> RootPaths
  -> PathIndex
  -> Eff e (DependencyTree, [Module SourceVariable SourceVariable])
parseAndMakeDependencyTree pathsToFilesToCompile rootPaths pathIndex = do
  Log.info
    "Start parsing an making dependency tree based on index"
    [ field "initial paths to compile" pathsToFilesToCompile
    , field "root paths" rootPaths
    , field "path index" pathIndex
    ]
  result <-
    loop pathsToFilesToCompile [] (makeEmptyDependencyTree rootPaths pathIndex)

  Log.info
    "Finished parsing and making dependency tree"
    [ field "initial paths to compile" pathsToFilesToCompile
    , field "root paths" rootPaths
    , field "path index" pathIndex
    , field "dependency tree" (fst result)
    , fieldWith
        (toJSON)
        (\x -> Pretty.list (format defaultFormatContext <$> x))
        "modules"
        (snd result)
    ]
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
  => Log :> e
  => DependencyTree
  -> [Module SourceVariable SourceVariable]
  -> Eff
      e
      (SymbolResolutionEnvironment, [Module ExpressionVariableId TypeVariableId])
solveSymbols _ _ = do
  Log.info "Symbol solver starts" []
  Log.info "Symbol solver ends" []
  pure (SymbolResolutionEnvironment, [])


typeCheckAndInference
  :: Reader CompilerConfig :> e
  => Accumulator OctizysWarn :> e
  => Accumulator OctizysError :> e
  => Log :> e
  => SymbolResolutionEnvironment
  -> [Module ExpressionVariableId TypeVariableId]
  -> Eff
      e
      (InferredTypesEnvironment, [AstModule TypeVariable])
typeCheckAndInference _ _ = do
  Log.info "Type check and type inference starts" []
  Log.info "Type check and type inference ends" []
  pure (InferredTypesEnvironment, [])


generateCode
  :: Reader CompilerConfig :> e
  => Log :> e
  => InferredTypesEnvironment
  -> [AstModule TypeVariable]
  -> Eff e ((), ())
generateCode _ _ = do
  Log.info "Code generation ends" []
  Log.info "Code generation ends" []
  pure ((), ())


-- TODO: add structure to errors so we can log them better
logErrors
  :: Log :> e
  => [OctizysError]
  -> Eff e ()
logErrors = mapM_ (\x -> Log.error "Error" [field "error message" x])


-- TODO: add structure to warns so we can log them better
logWarns
  :: Log :> e
  => [OctizysWarn]
  -> Eff e ()
logWarns =
  mapM_ (\x -> Log.warn "Warn" [field "warn message" x])


compileEffectful
  :: Reader CompilerConfig :> e
  => Accumulator OctizysWarn :> e
  => Error [OctizysError] :> e
  => FileReader :> e
  => Log :> e
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

compile :: NonEmpty FilePath -> Level -> RootPaths -> IO ()
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
--     , Log :> es
--     )
--  => FilePath
--  -> Eff es Ast.Type
-- compileFile = undefined
