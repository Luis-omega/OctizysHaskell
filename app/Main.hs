{-# LANGUAGE DataKinds #-}

module Main where

import Cli
  ( Command (CompileCmd, ReplCmd)
  , CompileOptions (CompileOptions, files, logLevel)
  , parseArguments
  )
import Octizys.Compiler.Compiler (compile)

-- import Octizys.Repl.Repl (runRepl)
import Options.Applicative (execParser, fullDesc, helper, info, (<**>))


main :: IO ()
main = do
  command <-
    execParser
      (info (parseArguments <**> helper) fullDesc)
  case command of
    ReplCmd opts -> print "repl disabled" -- runRepl opts
    CompileCmd (CompileOptions {logLevel = _logLevel, files = _files}) -> compile _files _logLevel
