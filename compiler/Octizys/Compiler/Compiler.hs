{-# LANGUAGE DataKinds #-}

module Octizys.Compiler.Compiler (compile) where

import Control.Arrow ((<<<))
import Control.Monad (void)
import Data.List.NonEmpty (NonEmpty ((:|)))
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
import Octizys.Effects.Logger.ConsoleInterpreter (runLog)
import Octizys.Effects.Logger.Effect (LogLevel, Logger)

-- import Octizys.FrontEnd.Cst.Expression (ExpressionVariableId)
import Octizys.FrontEnd.Cst.TopItem (Module)

-- import Octizys.Inference.ConstraintsGeneration (InferenceState)
-- import Octizys.Inference.ConstraintsSolver (solveDefinitionsType)

import Octizys.Common.Report (Report)
import Octizys.FrontEnd.Parser.TopItem (parseModule)

-- import Octizys.Pretty.FormatContext (Configuration)
-- import Octizys.Pretty.Formatter (format)
import Prettyprinter (Doc, Pretty (pretty))
import qualified Prettyprinter
import qualified Prettyprinter.Render.Text
import Text.Show.Pretty (ppShow)


compile :: NonEmpty FilePath -> LogLevel -> IO ()
compile paths logLevel = undefined

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
