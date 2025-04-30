module Main where

import Control.Arrow ((<<<))
import Effectful (runEff)
import Octizys.Effects.Console.Interpreter (runConsole)
import Octizys.Effects.SymbolResolution.Interpreter
  ( initialSymbolResolutionState
  , runSymbolResolutionFull
  )
import Octizys.Repl.Repl (repl)


main :: IO ()
main =
  run repl
  where
    run eff =
      fst
        <$> ( runEff
                <<< runConsole
                <<< runSymbolResolutionFull initialSymbolResolutionState
            )
          eff
