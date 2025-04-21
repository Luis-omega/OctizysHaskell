{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Repl.Repl (repl, runConsole) where

import Ast (Context)
import Effectful (Eff, (:>))
import Effectful.Error.Dynamic (Error, runErrorNoCallStackWith)
import Evaluation (EvaluationError, evaluateExpression)
import Parser (ParserError)
import Repl.Ast (ReplCommand (Quit), ReplTop (Command, Define, Evaluate))
import Repl.Console (Console, putLine, putString, readLine, runConsole)
import Repl.Parser (replParserEff)
import Text.Megaparsec (errorBundlePretty)


data ReplStatus = ContinueWith Context | Exit


continue :: Context -> Eff es ReplStatus
continue context = pure $ ContinueWith context


exit :: Eff es ReplStatus
exit = pure Exit


rep
  :: (Console :> es, Error ParserError :> es, Error EvaluationError :> es)
  => Context
  -> Eff es ReplStatus
rep context = do
  line <- putString "repl>" >> readLine
  action <- replParserEff line
  case action of
    Command Quit ->
      putLine "Bye!"
        >> exit
    Evaluate expression ->
      do
        (value, new_context) <- evaluateExpression context expression
        putLine (show value)
        continue new_context
    -- TODO: Type check/inference
    Define _ ->
      -- TODO: Raise a error
      putLine "Define is not supported yet!"
        >> continue context


reportError
  :: forall e es
   . (Console :> es, Show e)
  => Context
  -> Eff (Error e : es) ReplStatus
  -> Eff es ReplStatus
reportError context =
  runErrorNoCallStackWith
    (\e -> (putLine . show) e >> continue context)


reportParserError
  :: forall es
   . Console :> es
  => Context
  -> Eff (Error ParserError : es) ReplStatus
  -> Eff es ReplStatus
reportParserError context =
  runErrorNoCallStackWith
    (\e -> (putLine . errorBundlePretty) e >> continue context)


repl :: Console :> es => Context -> Eff es ()
repl context = do
  status <- reportErrors $ rep context
  case status of
    Exit -> pure ()
    ContinueWith new_context -> repl new_context
  where
    reportErrors = reportParserError context . reportError @EvaluationError context
