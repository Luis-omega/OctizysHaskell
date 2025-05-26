{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Octizys.Repl.Repl
  ( runRepl
  )
where

import Control.Arrow ((<<<))
import Effectful (Eff, runEff, (:>))
import Effectful.Error.Static
  ( Error
  , runErrorNoCallStack
  , runErrorNoCallStackWith
  )

-- import Octizys.Evaluation (EvaluationError)

import qualified Octizys.Inference.ConstraintsGeneration as Inference
import qualified Octizys.Inference.ConstraintsSolver as Inference
import qualified Octizys.Inference.Errors as Inference

import Data.Functor (void)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import Effectful.State.Static.Local (State, runState)
import Octizys.Ast.Evaluation (EvaluationError)
import qualified Octizys.Ast.Expression as Ast
import Octizys.Effects.Console.Effect
  ( Console
  , putText
  , readLine
  )
import Octizys.Effects.Console.Interpreter
  ( putLine
  , runConsole
  )
import Octizys.Effects.Logger.ConsoleInterpreter (runLog)
import Octizys.Effects.Logger.Effect (Logger)
import Octizys.Effects.Parser.Backend
  ( makeParseErrorReport
  )
import Octizys.Effects.Parser.Interpreter (runFullParser)
import Octizys.Effects.SymbolResolution.Effect
  ( SymbolResolution
  )
import Octizys.Effects.SymbolResolution.Interpreter
  ( SymbolResolutionError
  , initialSymbolResolutionState
  , runSymbolResolution
  )
import Octizys.Repl.Ast
  ( ReplCommand (LoadFile, Quit)
  , ReplTop (Command, Define, Evaluate)
  )
import Octizys.Repl.Parser (replParser)
import Prettyprinter
  ( Doc
  , Pretty (pretty)
  )
import Text.Show.Pretty (ppShow)

import Cli
  ( ReplOptions
      ( formatterConfig
      , logLevel
      , showCst
      , showInference
      )
  )
import Control.Monad (when)
import qualified Octizys.Ast.Evaluation as Evaluation
import Octizys.Classes.From (From (from))
import Octizys.Compiler.Format
  ( buildFormatContextFromSymbolResolution
  , buildInferenceErrorReport
  , pprint
  , render
  )
import Octizys.Compiler.StateSync
  ( DefinedSymbols
  , addDefinedSymbol
  , addExpressionSymbols
  , updateInferenceState
  , updateSymbolState
  )
import Octizys.Cst.Expression (ExpressionVariableId)
import qualified Octizys.Cst.Expression as Cst
import Octizys.Cst.Node (Node)
import Octizys.Pretty.FormatContext
  ( Configuration
  , FormatContext
  )
import Octizys.Pretty.Formatter (Formatter, format)


data ReplStatus = Continue | Exit


continue :: Eff es ReplStatus
continue = pure Continue


exit :: Eff es ReplStatus
exit = pure Exit


data CST = CExp Cst.Expression | CDef Cst.Definition
  deriving (Show, Eq, Ord)


instance From CST Cst.Expression where
  from = CExp


instance From CST Cst.Definition where
  from = CDef


instance From Node CST where
  from (CDef d) = from d
  from (CExp e) = from e


bimapCst
  :: forall a m
   . (Cst.Expression -> m a)
  -> (Cst.Definition -> m a)
  -> CST
  -> m a
bimapCst f1 f2 c =
  case c of
    CExp e -> f1 e
    CDef d -> f2 d


instance Formatter ann (FormatContext ann) CST where
  format ctx (CExp e) = format ctx e
  format ctx (CDef d) = format ctx d


data AST = AExp Ast.Expression | ADef Ast.Definition
  deriving (Show, Eq, Ord)


instance From AST Ast.Expression where
  from = AExp


instance From AST Ast.Definition where
  from = ADef


instance Formatter ann (FormatContext ann) AST where
  format ctx (AExp e) = format ctx e
  format ctx (ADef d) = format ctx d


bimapAST
  :: forall a m
   . (Ast.Expression -> m a)
  -> (Ast.Definition -> m a)
  -> AST
  -> m a
bimapAST f1 f2 c =
  case c of
    AExp e -> f1 e
    ADef d -> f2 d


cstTypeResolution
  :: forall es
   . ( State Inference.InferenceState :> es
     , Error Inference.InferenceError :> es
     , Logger :> es
     )
  => CST
  -> Eff es AST
cstTypeResolution =
  bimapCst
    (fmap AExp <<< Inference.solveExpressionType)
    (fmap ADef <<< Inference.solveDefinitionType)


addAstSymbols
  :: forall es
   . ( State Inference.InferenceState :> es
     , SymbolResolution :> es
     , Logger :> es
     , State DefinedSymbols :> es
     )
  => AST
  -> Eff es (Map ExpressionVariableId Ast.Expression)
addAstSymbols =
  bimapAST addExpressionSymbols addDefinedSymbol


processCst
  :: forall es
   . ( Console :> es
     , SymbolResolution :> es
     , State Inference.InferenceState :> es
     , Error EvaluationError :> es
     , State DefinedSymbols :> es
     , Logger :> es
     )
  => ReplOptions
  -> CST
  -> Eff es ReplStatus
processCst opts cst =
  let
    fmtConfig = opts.formatterConfig
   in
    do
      updateInferenceState
      when
        opts.showCst
        (pprint fmtConfig cst)
      maybeAst <-
        runErrorNoCallStack
          ( cstTypeResolution
              cst
          )
      case maybeAst of
        Left e -> do
          reportInferenceError
            fmtConfig
            (from cst)
            e
            >> continue
        Right ast -> do
          updateSymbolState
          when
            opts.showInference
            (pprint fmtConfig ast)
          context <- addAstSymbols ast
          case ast of
            AExp expr ->
              do
                result <- Evaluation.evaluateExpression context expr
                pprint fmtConfig result
            _ -> pure ()
          continue


rep
  :: ( Console :> es
     , SymbolResolution :> es
     , State Inference.InferenceState :> es
     , Error EvaluationError :> es
     , State DefinedSymbols :> es
     , Logger :> es
     )
  => ReplOptions
  -> Eff es ReplStatus
rep opts = do
  line <- putText "repl>" >> readLine
  maybeAction <- runFullParser line replParser
  -- s12 <- SRS.getSymbolResolutionState
  -- putLine $ pack $ ppShow s12.expNamesToId
  case maybeAction of
    Left e -> do
      ctx <- buildFormatContextFromSymbolResolution opts.formatterConfig
      putLine $
        render $
          format
            ctx
            ( makeParseErrorReport
                ctx
                (Just ('R' :| "epl"))
                line
                e
            )
      continue
    Right action ->
      case action of
        Command Quit ->
          putLine "Bye!"
            >> exit
        Command (LoadFile f) -> do
          putLine $ "Unsupported load of file: " <> f
          continue
        Evaluate expression ->
          processCst opts (from expression)
        Define d ->
          processCst opts (from d)


reportInferenceError
  :: SymbolResolution :> es
  => Console :> es
  => Configuration
  -> Node
  -> Inference.InferenceError
  -> Eff es ()
reportInferenceError config cst err = do
  ctx <- buildFormatContextFromSymbolResolution config
  let re = buildInferenceErrorReport ctx cst err
  putLine $ render $ format ctx re


reportErrorWith
  :: forall e es ann
   . Console :> es
  => (e -> Doc ann)
  -> Eff (Error e : es) ReplStatus
  -> Eff es ReplStatus
reportErrorWith prettier =
  runErrorNoCallStackWith
    (\e -> (putLine <<< render <<< prettier) e >> continue)


reportErrorShow
  :: forall e es
   . (Console :> es, Show e)
  => Eff (Error e : es) ReplStatus
  -> Eff es ReplStatus
reportErrorShow = reportErrorWith (pretty <<< ppShow)


repl
  :: Console :> es
  => Logger :> es
  => ReplOptions
  -> Eff es ()
repl opts =
  ( void
      <<< runState @DefinedSymbols mempty
      <<< runState Inference.initialInferenceState
      <<< runState initialSymbolResolutionState
  )
    loop
  where
    loop = do
      status <-
        ( reportErrorShow @SymbolResolutionError
            <<< reportErrorShow @EvaluationError
            <<< runSymbolResolution
          )
          (rep opts)
      case status of
        Exit -> pure ()
        Continue -> loop


runRepl :: ReplOptions -> IO ()
runRepl options =
  ( runEff
      <<< runConsole
      <<< runLog options.logLevel
  )
    (repl options)
