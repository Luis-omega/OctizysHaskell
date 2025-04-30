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
  )
where

import Control.Arrow ((<<<))
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, runErrorNoCallStackWith)

-- import Octizys.Evaluation (EvaluationError)
-- import Octizys.Inference.Translation
--   ( TranslationState
--   , TranslationWarning
--   , buildInferenceContext
--   )
-- import qualified Octizys.Inference.Translation as Inference

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Prettyprint.Doc.Render.Text as Prettyprinter.Render.Text
import Octizys.Effects.Console.Effect
  ( Console
  , putText
  , readLine
  )
import Octizys.Effects.Console.Interpreter
  ( putLine
  , runConsole
  )
import Octizys.Effects.Parser.Backend (ParserError, makeInitialState)
import Octizys.Effects.Parser.Interpreter (runParserWith)
import Octizys.Effects.SymbolResolution.Effect (SymbolResolution)
import Octizys.Parser.Common (OctizysParseError)
import Octizys.Pretty.Expression (prettyExpression)
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
     , Error (ParserError OctizysParseError) :> es
     , SymbolResolution :> es
     )
  => Eff es ReplStatus
rep = do
  line <- putText "repl>" >> readLine
  let newParserState = makeInitialState line
  (action, _) <- runParserWith newParserState replParser
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
        -- TODO: solve this
        -- (value, new_context) <- evaluateExpression context expression
        -- putLine (show value)
        -- continue new_context
        continue
    -- TODO: Type check/inference
    Define d -> do
      putLine $ render (prettyModule pretty pretty) d
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

reportError
  :: forall e es
   . (Console :> es, Show e)
  => Eff (Error e : es) ReplStatus
  -> Eff es ReplStatus
reportError =
  runErrorNoCallStackWith
    (\e -> (putLine <<< Text.pack <<< show) e >> continue)


repl :: SymbolResolution :> es => Console :> es => Eff es ()
repl = do
  status <- reportError rep
  case status of
    Exit -> pure ()
    Continue -> repl
