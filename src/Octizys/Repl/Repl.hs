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
import Effectful.Error.Static (Error, catchError, runErrorNoCallStackWith)

-- import Octizys.Evaluation (EvaluationError)

import qualified Octizys.Effects.SymbolResolution.Effect as SRS
import qualified Octizys.Inference.Inference as Inference

import Data.Functor (void)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text, pack)
import Effectful.State.Static.Local (State, get, put, runState)
import Octizys.Effects.Console.Effect
  ( Console
  , putText
  , readLine
  )
import Octizys.Effects.Console.Interpreter
  ( putLine
  , runConsole
  )
import Octizys.Effects.Parser.Backend
  ( ParserError
  , prettyParserError
  )
import Octizys.Effects.Parser.Interpreter (runFullParser)
import Octizys.Effects.SymbolResolution.Effect
  ( SymbolResolution
  , SymbolResolutionState
  , getSymbolResolutionState
  , putSymbolResolutionState
  )
import Octizys.Effects.SymbolResolution.Interpreter
  ( SymbolResolutionError
  , initialSymbolResolutionState
  , runSymbolResolution
  , runSymbolResolutionFull
  )
import Octizys.Inference.Inference (definitionCstToAst)
import Octizys.Parser.Common (OctizysParseError)
import Octizys.Pretty.Expression (prettyDefinition, prettyExpression)
import Octizys.Pretty.TopItem (prettyModule)
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


rep
  :: ( Console :> es
     , SymbolResolution :> es
     , State Inference.InferenceState :> es
     , Error Inference.InferenceError :> es
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
            putLine $ render (prettyExpression pretty pretty) expression
            updateInferenceState
            (ast, _type) <- Inference.infer expression
            updateSymbolState
            putLine $ pack $ ppShow ast
            putLine $ pack $ ppShow _type
            -- TODO: solve this
            -- (value, new_context) <- evaluateExpression context expression
            -- putLine (show value)
            -- continue new_context
            continue
        -- TODO: Type check/inference
        Define d -> do
          putLine $ render (prettyDefinition pretty pretty) d
          updateInferenceState
          ast <- definitionCstToAst d
          putLine $ pack $ show ast
          updateSymbolState
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
          , Inference.nextTypeVar = SRS.genVarType currentSymbolState + 1
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
          , SRS.genVarType = Inference.nextTypeVar currentInference + 1
          }


-- TODO: do this again
-- (_, newTranslationState) <-
--   runState
--     (translationState context)
--     (buildInferenceContext [d])
-- -- TODO: Raise a error
-- putLine $ "New State:\n" <> ppShow newTranslationState
-- putLine "Define is not supported yet!"
--   >> continue
--     ( context
--         { translationState =
--             newTranslationState
--         }
--     )

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


repl
  :: Console :> es
  => Eff es ()
repl =
  ( void
      <<< runState Inference.initialInferenceState
      <<< runState initialSymbolResolutionState
  )
    loop
  where
    loop = do
      status <-
        ( reportErrorShow @SymbolResolutionError
            <<< reportErrorShow @Inference.InferenceError
            <<< runSymbolResolution
          )
          rep
      case status of
        Exit -> pure ()
        Continue -> loop
