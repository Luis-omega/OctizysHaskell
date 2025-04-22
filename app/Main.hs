module Main where

import Effectful (runEff)
import Repl.Console (runConsole)
import Repl.Repl (emptyState, repl)


main :: IO ()
main =
  run $ repl emptyState
  where
    run = runEff . runConsole
