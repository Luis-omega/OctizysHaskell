{-# LANGUAGE DataKinds #-}

module Main where

import Cli
  ( Command (CompileCmd, ReplCmd)
  , CompileOptions (CompileOptions, files, logLevel)
  , parseArguments
  )
import Octizys.Compiler.Compiler (compile)
import Octizys.PathResolution.PathIndex (makeRootPaths)

-- import Octizys.Repl.Repl (runRepl)
import Options.Applicative (execParser, fullDesc, helper, info, (<**>))


main :: IO ()
main = do
  command <-
    execParser
      (info (parseArguments <**> helper) fullDesc)
  case command of
    ReplCmd _opts -> print @String "repl disabled" -- runRepl opts
    CompileCmd (CompileOptions {logLevel = _logLevel, files = _files}) ->
      let roots = makeRootPaths []
       in compile _files _logLevel roots
