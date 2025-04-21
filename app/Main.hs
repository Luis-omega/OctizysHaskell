module Main where

import Ast (makeEmptyContext)
import Effectful (runEff)
import Repl.Console (runConsole)
import Repl.Repl (repl)


main :: IO ()
main =
  let empty_context = makeEmptyContext
   in run $ repl empty_context
  where
    run = runEff . runConsole
