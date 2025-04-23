module Main where

import Effectful (runEff)
import Octizys.Repl.Console (runConsole)
import Octizys.Repl.Repl (emptyState, repl)


main :: IO ()
main =
  run $ repl emptyState
  where
    run = runEff . runConsole
