{-# LANGUAGE DataKinds #-}

module Octizys.Compiler.Compiler (compile) where

import Control.Arrow ((<<<))
import Control.Monad (void)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Text (Text)
import Effectful (Eff, runEff)
import Effectful.Console.ByteString (Console)
import Effectful.Error.Static (Error, runErrorNoCallStackWith)
import Effectful.Internal.Effect ((:>))
import Effectful.State.Static.Local
import Octizys.Ast.Evaluation (EvaluationError)
import qualified Octizys.Ast.Expression as Ast
import qualified Octizys.Ast.Type as Ast
import Octizys.Cst.Expression (ExpressionVariableId)
import Octizys.Effects.Console.Interpreter (putLine, runConsole)
import Octizys.Effects.Logger.ConsoleInterpreter (runLog)
import Octizys.Effects.Logger.Effect (LogLevel, Logger)
import Octizys.Effects.SymbolResolution.Effect (SymbolResolution)
import Octizys.Effects.SymbolResolution.Interpreter
  ( SymbolResolutionError
  , initialSymbolResolutionState
  , runSymbolResolution
  )
import Prettyprinter (Doc, Pretty (pretty))
import qualified Prettyprinter
import qualified Prettyprinter.Render.Text
import Text.Show.Pretty (ppShow)


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

compile :: NonEmpty FilePath -> LogLevel -> IO ()
compile paths logLevel = undefined

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
