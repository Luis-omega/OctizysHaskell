{-# LANGUAGE DataKinds #-}

module Main where

import Cli (Command (CompileCmd, ReplCmd), parseArguments)
import Octizys.Compiler.Compiler (compile)
import Octizys.Repl.Repl (runRepl)
import Options.Applicative (execParser, fullDesc, helper, info, (<**>))


main :: IO ()
main = do
  command <-
    execParser
      (info (parseArguments <**> helper) fullDesc)
  case command of
    ReplCmd opts -> runRepl opts
    CompileCmd _ -> compile

