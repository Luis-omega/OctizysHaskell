{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Octizys.Repl.Repl
  ( repl
  , runConsole
  , emptyState
  , ReplState (ReplState, translationState)
  , render
  )
where

import Control.Arrow ((<<<))
import Effectful (Eff, (:>))
import Effectful.Error.Dynamic (Error, runErrorNoCallStackWith)
import Effectful.State.Static.Local (runState)
import Effectful.Writer.Static.Local (Writer, runWriter)
import Octizys.Evaluation (EvaluationError)
import Octizys.Inference
  ( TranslationState
  , TranslationWarning
  , buildInferenceContext
  )
import qualified Octizys.Inference as Inference
import Octizys.Parser (ParserError)
import Octizys.Repl.Ast
  ( ReplCommand (LoadFile, Quit)
  , ReplTop (Command, Define, Evaluate)
  )
import Octizys.Repl.Console
  ( Console
  , putLine
  , putString
  , readLine
  , runConsole
  )
import Octizys.Repl.Parser (replParserEff)
import Prettyprinter (Pretty (pretty), defaultLayoutOptions, layoutPretty)
import qualified Prettyprinter.Render.String
import Text.Megaparsec (errorBundlePretty)
import Text.Show.Pretty (ppShow)


newtype ReplState = ReplState
  { translationState :: TranslationState
  }


data ReplStatus = ContinueWith ReplState | Exit


emptyState :: ReplState
emptyState =
  ReplState
    { translationState = Inference.emptyState
    }


continue :: ReplState -> Eff es ReplStatus
continue = pure <<< ContinueWith


exit :: Eff es ReplStatus
exit = pure Exit


render :: forall a. Pretty a => a -> String
render =
  Prettyprinter.Render.String.renderString
    <<< Prettyprinter.layoutPretty Prettyprinter.defaultLayoutOptions
    <<< pretty


rep
  :: ( Console :> es
     , Error ParserError :> es
     , Error EvaluationError :> es
     , Error Inference.TranslationError
        :> es
     , Writer
        [Inference.TranslationWarning]
        :> es
     )
  => ReplState
  -> Eff es ReplStatus
rep context = do
  line <- putString "repl>" >> readLine
  action <- replParserEff line
  case action of
    Command Quit ->
      putLine "Bye!"
        >> exit
    Command (LoadFile f) -> do
      putLine $ "Unsupporte load of file: " <> f
      continue context
    Evaluate expression ->
      do
        putLine $ render expression
        -- TODO: solve this
        -- (value, new_context) <- evaluateExpression context expression
        -- putLine (show value)
        -- continue new_context
        continue context
    -- TODO: Type check/inference
    Define d -> do
      putLine $ render d
      (_, newTranslationState) <-
        runState
          (translationState context)
          (buildInferenceContext [d])
      -- TODO: Raise a error
      putLine $ "New State:\n" <> ppShow newTranslationState
      putLine "Define is not supported yet!"
        >> continue
          ( context
              { translationState =
                  newTranslationState
              }
          )


reportError
  :: forall e es
   . (Console :> es, Show e)
  => ReplState
  -> Eff (Error e : es) ReplStatus
  -> Eff es ReplStatus
reportError context =
  runErrorNoCallStackWith
    (\e -> (putLine <<< show) e >> continue context)


reportParserError
  :: forall es
   . Console :> es
  => ReplState
  -> Eff (Error ParserError : es) ReplStatus
  -> Eff es ReplStatus
reportParserError context =
  runErrorNoCallStackWith
    (\e -> (putLine <<< errorBundlePretty) e >> continue context)


repl :: Console :> es => ReplState -> Eff es ()
repl context = do
  (status, warnings) <- reportWarnings $ reportErrors $ rep context
  mapM_ (putLine <<< show) warnings
  case status of
    Exit -> pure ()
    ContinueWith new_context -> repl new_context
  where
    reportErrors =
      reportError context
        <<< reportParserError context
        <<< reportError @EvaluationError context
    reportWarnings =
      runWriter @[TranslationWarning]
