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
import Effectful.Error.Static (Error, runErrorNoCallStackWith)

-- import Octizys.Evaluation (EvaluationError)
import qualified Octizys.Inference.Inference as Inference
import qualified Octizys.Effects.SymbolResolution.Effect as SRS

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text,pack)
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
import Octizys.Effects.SymbolResolution.Effect (SymbolResolution, getSymbolResolutionState, SymbolResolutionState, putSymbolResolutionState)
import Octizys.Parser.Common (OctizysParseError)
import Octizys.Pretty.Expression (prettyExpression, prettyDefinition)
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
import qualified Octizys.Inference.Inference as Inference
import Effectful.State.Static.Local (State, runState)
import qualified Data.Map as Map
import Octizys.Inference.Inference (definitionCstToAst)


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
            (ast,_type) <- Inference.infer expression
            putLine $ pack $ show ast
            putLine $ pack $ show _type
            -- TODO: solve this
            -- (value, new_context) <- evaluateExpression context expression
            -- putLine (show value)
            -- continue new_context
            continue
        -- TODO: Type check/inference
        Define d -> do
          putLine $ render (prettyDefinition pretty pretty) d
          ast <- definitionCstToAst d
          putLine $ pack $ show ast
          continue


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
reportErrorShow = reportErrorWith (pretty <<< show)


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

makeInferenceState ::
  SymbolResolutionState -> Inference.InferenceState
makeInferenceState srs =
  Inference.InferenceStateC
    { Inference.expVarTable = SRS.expVarTable srs
    , Inference.typeVarToType = Map.empty
    , Inference.realVariablesMax = SRS.genVarExp srs
    , Inference.nextTypeVar = SRS.genVarType srs
    }


-- TODO: verify change of state
makeSymbolResolutionState ::
  Inference.InferenceState -> SymbolResolutionState -> SymbolResolutionState
makeSymbolResolutionState is old =
  old
    { SRS.expVarTable = Inference.expVarTable is
    , SRS.genVarExp = Inference.realVariablesMax is
    , SRS.genVarType = Inference.nextTypeVar is
    }

repl
  :: SymbolResolution :> es
  => Console :> es
  => Eff es ()
repl = do
  symbolState <- getSymbolResolutionState
  let newInferenceState = makeInferenceState symbolState
  (status,inferState) <-
    runState
    newInferenceState
    (reportErrorShow rep)
  let newSymbolState = makeSymbolResolutionState  inferState symbolState
  putSymbolResolutionState newSymbolState
  case status of
    Exit -> pure ()
    Continue -> repl
