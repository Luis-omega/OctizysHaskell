{-# LANGUAGE DataKinds #-}

module Octizys.Compiler.Compiler (compile, CompilerConfig (CompilerConfig')) where

import Control.Arrow ((<<<))
import Control.Monad (forM_, unless, void)
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Effectful (Eff, IOE, MonadIO (liftIO), runEff, runPureEff)
import Effectful.Console.ByteString (Console)
import Effectful.Error.Static
  ( Error
  , runError
  , runErrorNoCallStack
  , runErrorNoCallStackWith
  , throwError
  , tryError
  )
import Effectful.Internal.Effect ((:>))
import Effectful.Reader.Static (Reader, runReader)
import Effectful.State.Static.Local
import EffectfulParserCombinators.Interpreter
  ( runFullParser
  , runParser
  , runParserWith
  )
import EffectfulParserCombinators.Span (makeInitialPosition)
import Octizys.Ast.Evaluation (EvaluationError)
import qualified Octizys.Ast.Expression as Ast
import qualified Octizys.Ast.Type as Ast

-- import Octizys.Compiler.Format
--   ( buildFormatContext
--   , buildInferenceErrorReport
--   , formatE
--   )
import Octizys.Effects.Console.Interpreter (putLine, runConsole)
import Octizys.Effects.Logger.ConsoleInterpreter (errorLog, runLog)
import Octizys.Effects.Logger.Effect (LogLevel, Logger, info, warn)

-- import Octizys.FrontEnd.Cst.Expression (ExpressionVariableId)
import Octizys.FrontEnd.Cst.TopItem (Module)

-- import Octizys.Inference.ConstraintsGeneration (InferenceState)
-- import Octizys.Inference.ConstraintsSolver (solveDefinitionsType)

import Octizys.Common.Report (Report)
import Octizys.FrontEnd.Parser.TopItem (parseModule)

-- import Octizys.Pretty.FormatContext (Configuration)
-- import Octizys.Pretty.Formatter (format)

import Control.Exception (IOException)
import Data.Either (partitionEithers)
import EffectfulParserCombinators.Error (ParserError, humanReadableError)
import GHC.Base (when)
import Octizys.Ast.Type (TypeVariable)
import Octizys.Classes.From (From (from))
import Octizys.Common.Id (ExpressionVariableId, TypeVariableId)
import Octizys.Effects.Accumulator.Effect (Accumulator, accumulate)
import Octizys.Effects.Accumulator.Interpreter (runAccumulatorFull)
import Octizys.Effects.FileReader.Effect (FileReadError, FileReader)
import qualified Octizys.Effects.FileReader.Effect as FileReader
import Octizys.Effects.FileReader.Interpreter (runFileReader)
import Octizys.FrontEnd.Cst.SourceInfo (SourceVariable)
import Octizys.FrontEnd.Parser.Common (OctizysParseError)
import Octizys.Scope (ImportsScope)
import Prettyprinter (Doc, Pretty (pretty))
import qualified Prettyprinter
import qualified Prettyprinter.Render.Text
import Text.Show.Pretty (ppShow)


data OctizysError
  = OctizysParserError FilePath Text (ParserError OctizysParseError)
  | OctizysFileReadError FileReadError
  deriving (Show, Eq)


instance From OctizysError FileReadError where
  from = OctizysFileReadError


instance Pretty (OctizysError) where
  pretty (OctizysParserError _ src parseError) =
    humanReadableError Nothing src parseError
  pretty (OctizysFileReadError e) = pretty e


data OctizysWarn = OctizysWarn
  deriving (Show, Eq, Ord)


data CompilerConfig = CompilerConfig'
  deriving (Show, Eq, Ord)


data DependencyTree = DependencyTree


data AstModule a = AstModule


data SymbolResolutionEnvironment = SymbolResolutionEnvironment


data InferredTypesEnvironment = InferredTypesEnvironment


parseAndMakeDependencyTree
  :: Reader CompilerConfig :> e
  => Accumulator OctizysWarn :> e
  => Accumulator OctizysError :> e
  => FileReader :> e
  => Logger :> e
  => [FilePath]
  -> Eff e ([Module SourceVariable SourceVariable], DependencyTree)
parseAndMakeDependencyTree paths = do
  info (pretty ("Compiling paths " <> show paths))
  maybeSourceCodes <-
    mapM
      ( \p ->
          do
            result <- FileReader.readFile p
            pure (p, result)
      )
      paths
  let (readingErrors, sourceCodes) = partition maybeSourceCodes
  forM_
    ( (\(_, x) -> from x)
        <$> readingErrors
    )
    (accumulate @OctizysError)
  maybeCSTs <-
    mapM
      ( \(p, x) ->
          do
            result <- runFullParser x parseModule
            pure ((p, x), result)
      )
      sourceCodes
  let (parsingErrors, csts) = partition maybeCSTs
      octizysParsingErrors :: [OctizysError] =
        ( \((p, x), e) ->
            OctizysParserError p x e
        )
          <$> parsingErrors
  forM_
    octizysParsingErrors
    (accumulate @OctizysError)
  -- TODO: add the file path to the module data type and don't
  -- discard it here.
  pure (snd <$> csts, DependencyTree)
  where
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
      ([Module ExpressionVariableId TypeVariableId], SymbolResolutionEnvironment)
solveSymbols _ _ = do
  info (pretty @Text "Solving symbols")
  pure ([], SymbolResolutionEnvironment)


typeCheckAndInference
  :: Reader CompilerConfig :> e
  => Accumulator OctizysWarn :> e
  => Accumulator OctizysError :> e
  => Logger :> e
  => SymbolResolutionEnvironment
  -> [Module ExpressionVariableId TypeVariableId]
  -> Eff
      e
      ([AstModule TypeVariable], InferredTypesEnvironment)
typeCheckAndInference _ _ = do
  info (pretty @Text "Inferring and checking types")
  pure ([], InferredTypesEnvironment)


generateCode
  :: Reader CompilerConfig :> e
  => Logger :> e
  => InferredTypesEnvironment
  -> [AstModule TypeVariable]
  -> Eff e ()
generateCode _ _ = do
  info (pretty @Text "Generating Code")
  pure ()


logErrors
  :: Logger :> e
  => [OctizysError]
  -> Eff e ()
logErrors errors =
  errorLog (pretty errors)


logWarns
  :: Logger :> e
  => [OctizysWarn]
  -> Eff e ()
logWarns _ = errorLog (pretty @Text "Logging Warns!")


compileEffectful
  :: Reader CompilerConfig :> e
  => Accumulator OctizysWarn :> e
  => Error [OctizysError] :> e
  => FileReader :> e
  => Logger :> e
  => NonEmpty FilePath
  -> Eff e ()
compileEffectful paths =
  throwOrAdvance
    generateCode
    ( throwOrAdvance
        typeCheckAndInference
        (throwOrAdvance solveSymbols $ parseAndMakeDependencyTree (toList paths))
    )
  where
    throwOrAdvance nextStep currentStep = do
      ((modules, environment), errors) <-
        runAccumulatorFull @OctizysError [] currentStep
      unless (null errors) (throwError errors)
      nextStep environment modules


compile :: NonEmpty FilePath -> LogLevel -> IO ()
compile paths level =
  runEff
    ( runFileReader
        ( runConsole
            (runLog level (runErrorsAndWarns (compileEffectful paths)))
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
--  { definedSymbols :: Map ExpressionVariableId Ast.Expression
--  }
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
