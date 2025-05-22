{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Octizys.Repl.Repl
  ( repl
  , runConsole
  , render
  , reportErrorShow
  , reportErrorParser
  )
where

import Control.Arrow ((<<<))
import Effectful (Eff, (:>))
import Effectful.Error.Static
  ( Error
  , runErrorNoCallStackWith
  )

-- import Octizys.Evaluation (EvaluationError)

import qualified Octizys.Effects.SymbolResolution.Effect as SRS
import Octizys.Inference.Inference (definitionCstToAst)
import qualified Octizys.Inference.Inference as Inference

import Data.Functor (void)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text, pack)
import Effectful.State.Static.Local (State, get, gets, put, runState)
import qualified Octizys.Ast.Expression as Ast
import qualified Octizys.Cst.Expression as Cst
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
import qualified Octizys.Effects.Logger.Effect as Logger
import Octizys.Effects.Parser.Backend
  ( ParserError
  , prettyParserError
  )
import Octizys.Effects.Parser.Interpreter (runFullParser)
import Octizys.Effects.SymbolResolution.Effect
  ( SymbolResolution
  , putSymbolResolutionState
  )
import Octizys.Effects.SymbolResolution.Interpreter
  ( SymbolResolutionError
  , initialSymbolResolutionState
  , runSymbolResolution
  )
import Octizys.Parser.Common (OctizysParseError)
import qualified Octizys.Pretty.Expression as Cst
import Octizys.Repl.Ast
  ( ReplCommand (LoadFile, Quit)
  , ReplTop (Command, Define, Evaluate)
  )
import Octizys.Repl.Parser (replParser)
import Prettyprinter
  ( Doc
  , Pretty (pretty)
  , defaultLayoutOptions
  , layoutPretty
  )
import qualified Prettyprinter.Render.Text
import Text.Show.Pretty (ppShow)
import Octizys.Ast.Evaluation (EvaluationError, EvaluationState)

import qualified Octizys.Ast.Evaluation  as Evaluation

data ReplStatus = Continue | Exit


continue :: Eff es ReplStatus
continue = pure Continue


exit :: Eff es ReplStatus
exit = pure Exit


render :: forall a ann. (a -> Doc ann) -> a -> Text
render prettifier =
  Prettyprinter.Render.Text.renderStrict
    <<< Prettyprinter.layoutPretty Prettyprinter.defaultLayoutOptions
    <<< prettifier


prettyExpression
  :: State Inference.InferenceState :> es
  => Ast.Expression
  -> Eff es (Doc ann)
prettyExpression e = do
  ist <- gets Inference.expVarTable
  pure $
    Ast.prettyExpressionWithDic
      ist
      (\x -> pretty @Text x.name)
      e


prettyCstExpression
  :: SymbolResolution :> es
  => Cst.Expression
  -> Eff es (Doc ann)
prettyCstExpression e = do
  srs <- SRS.getSymbolResolutionState
  pure $
    Cst.prettyExpressionWithDic
      srs.expVarTable
      (\x -> pretty @Text x.name)
      e


rep
  :: ( Console :> es
     , SymbolResolution :> es
     , State Inference.InferenceState :> es
     , Error Inference.InferenceError :> es
     , Error EvaluationError :> es
     , State EvaluationState :> es
     , Logger :> es
     )
  => Eff es ReplStatus
rep = do
  line <- putText "repl>" >> readLine
  maybeAction <- runFullParser line replParser
  case maybeAction of
    Left e -> do
      putLine $
        render
          ( prettyParserError
              pretty
              (Just ('R' :| "epl"))
              line
          )
          e
      continue
    Right action ->
      case action of
        Command Quit ->
          putLine "Bye!"
            >> exit
        Command (LoadFile f) -> do
          putLine $ "Unsupporte load of file: " <> f
          continue
        Evaluate expression ->
          do
            docExp <- prettyCstExpression expression
            putLine $ render id docExp
            updateInferenceState
            final <- Inference.solveExpressionType expression
            updateSymbolState
            updateEvaluationState
            docFinal <- prettyExpression final
            putLine $ render id docFinal
            -- ist <- gets Inference.expVarTable
            -- putLine $ pack $ ppShow (Map.toList ist)
            -- TODO: solve this
            -- (value, new_context) <- evaluateExpression context expression
            -- putLine (show value)
            -- continue new_context
            continue
        -- TODO: Type check/inference
        Define d -> do
          putLine $ render (Cst.prettyDefinition pretty pretty) d
          updateInferenceState
          ast <- definitionCstToAst d
          putLine $ pack $ show ast
          updateSymbolState
          updateEvaluationState
          continue
  where
    updateInferenceState
      :: SymbolResolution :> es
      => State Inference.InferenceState :> es
      => Eff es ()
    updateInferenceState = do
      currentSymbolState <- SRS.getSymbolResolutionState
      currentInference <- get
      put
        currentInference
          { Inference.expVarTable = SRS.expVarTable currentSymbolState
          , Inference.realVariablesMax =
              SRS.genVarType currentSymbolState + 1
          , Inference.constraintMap =
              currentInference.constraintMap
                { Inference.nextTypeVar = SRS.genVarType currentSymbolState + 1
                }
          }
    updateSymbolState
      :: SymbolResolution :> es
      => State Inference.InferenceState :> es
      => Eff es ()
    updateSymbolState = do
      currentInference <- get
      currentSymbolState <- SRS.getSymbolResolutionState
      putSymbolResolutionState
        currentSymbolState
          { SRS.expVarTable = Inference.expVarTable currentInference
          , SRS.genVarType =
              currentInference.constraintMap.nextTypeVar
                + 1
          }
    updateEvaluationState
      :: State EvaluationState :> es
      => State Inference.InferenceState :> es
      => Eff es ()
    updateEvaluationState = do
      currentInference <- gets @Inference.InferenceState Inference.expVarTable
      currentEvaluationState <- get @EvaluationState
      put
        currentEvaluationState
          { Evaluation.varToExp =undefined currentInference
          }


reportErrorWith
  :: forall e es ann
   . Console :> es
  => (e -> Doc ann)
  -> Eff (Error e : es) ReplStatus
  -> Eff es ReplStatus
reportErrorWith prettier =
  runErrorNoCallStackWith
    (\e -> (putLine <<< render prettier) e >> continue)


reportErrorShow
  :: forall e es
   . (Console :> es, Show e)
  => Eff (Error e : es) ReplStatus
  -> Eff es ReplStatus
reportErrorShow = reportErrorWith (pretty <<< ppShow)


reportErrorPretty
  :: forall e es
   . (Console :> es, Pretty e)
  => Eff (Error e : es) ReplStatus
  -> Eff es ReplStatus
reportErrorPretty = reportErrorWith pretty


reportErrorParser
  :: forall es
   . Console :> es
  => Text
  -> Eff
      ( Error
          ( ParserError OctizysParseError
          )
          : es
      )
      ReplStatus
  -> Eff es ReplStatus
reportErrorParser source =
  reportErrorWith
    ( prettyParserError
        pretty
        (Just ('R' :| "epl"))
        source
    )


-- TODO: add good prettifier with a dictionary
repl
  :: Console :> es
  => Eff es ()
repl =
  ( void
      <<< runLog Logger.Error
      <<< runState Evaluation.initialEvaluationState
      <<< runState Inference.initialInferenceState
      <<< runState initialSymbolResolutionState
  )
    loop
  where
    loop = do
      status <-
        ( reportErrorShow @SymbolResolutionError
            <<< reportErrorPretty @EvaluationError
            <<< reportErrorPretty @Inference.InferenceError
            <<< runSymbolResolution
          )
          rep
      case status of
        Exit -> pure ()
        Continue -> loop
