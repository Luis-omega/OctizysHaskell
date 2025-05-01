{-# LANGUAGE DataKinds #-}

module Main where

import Control.Arrow ((<<<))
import qualified Data.Text as Text
import Effectful (Eff, runEff, (:>))
import Effectful.Error.Static (Error, runErrorNoCallStackWith)
import Octizys.Effects.Console.Effect (Console)
import Octizys.Effects.Console.Interpreter (putLine, runConsole)
import Octizys.Effects.SymbolResolution.Interpreter
  ( SymbolResolutionError
  , initialSymbolResolutionState
  , runSymbolResolutionFull
  )
import Octizys.Repl.Repl (repl)


reportErrorShow
  :: Show e
  => Console :> es
  => Eff (Error e : es) a
  -> Eff es ()
reportErrorShow ef =
  runErrorNoCallStackWith
    (putLine <<< Text.pack <<< show)
    (do _ <- ef; pure ())


main :: IO ()
main = do
  _ <- run repl
  pure ()
  where
    run =
      runEff
        <<< runConsole
        <<< reportErrorShow @SymbolResolutionError
        <<< runSymbolResolutionFull initialSymbolResolutionState
