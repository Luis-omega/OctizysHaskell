{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module Octizys.Compiler.Compiler
  ( compile
  ) where

import Control.Monad (unless)
import Effectful (runEff)
import Effectful.Error.Static (runError)
import Effectful.Internal.Effect ((:>))
import Effectful.Internal.Monad (Eff)
import qualified Octizys.Compiler.Error as Octizys
import Octizys.Compiler.Pipeline (compilePackage)
import qualified Octizys.Compiler.Warn as Octizys
import Octizys.Effects.Accumulator.Interpreter (runAccumulatorFull)
import Octizys.Effects.Console.Interpreter (runConsole)
import Octizys.Effects.FileReader.Interpreter (runFileReader)
import Octizys.Logging.Effect (Log)
import Octizys.Logging.Entry (field)
import Octizys.Logging.Interpreters.Console (runLog)
import Octizys.Logging.Levels (Level)
import qualified Octizys.Logging.Loggers as Log


-- TODO: add structure to errors so we can log them better
logErrors
  :: Log :> e
  => [Octizys.Error]
  -> Eff e ()
logErrors = mapM_ (\x -> Log.error "Error" [field "error message" x])


-- TODO: add structure to warns so we can log them better
logWarns
  :: Log :> e
  => [Octizys.Warn]
  -> Eff e ()
logWarns =
  mapM_ (\x -> Log.warn "Warn" [field "warn message" x])


compile :: FilePath -> Level -> IO ()
compile paths level =
  runEff
    ( runFileReader
        ( runConsole
            ( runLog
                level
                ( runErrorsAndWarns (compilePackage paths undefined)
                )
            )
        )
    )
  where
    runErrorsAndWarns eff = do
      (_, warns) <-
        runAccumulatorFull
          []
          ( do
              maybeNoErrors <- runError @[Octizys.Error] eff
              case maybeNoErrors of
                Left (_, errors) -> logErrors errors
                Right _ -> pure ()
          )
      unless (null warns) (logWarns warns)
