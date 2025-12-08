module Cli
  ( Command (..)
  , ReplOptions (..)
  , CompileOptions (..)
  , parseArguments
  ) where

import qualified Data.Char as Char
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Octizys.Logging.Levels (Level (Debug, Error, Info, Trace))
import Octizys.Pretty.FormatContext (Configuration, makeConfiguration)
import Options.Applicative


data Command
  = ReplCmd ReplOptions
  | CompileCmd CompileOptions
  deriving (Show)


data ReplOptions = ReplOptions
  { logLevel :: Level
  , showCst :: Bool
  , showInference :: Bool
  , formatterConfig :: Configuration
  }
  deriving (Show)


data CompileOptions = CompileOptions
  { files :: NonEmpty FilePath
  , logLevel :: Level
  }
  deriving (Show)


parseLevel :: Parser Level
parseLevel =
  option
    (eitherReader readLevel)
    ( long "logLevel"
        <> metavar "LOG_LEVEL"
        <> value Info
        <> help "Set log level: trace, debug, info, error (default: info)."
    )


readLevel :: String -> Either String Level
readLevel s = case map toLower s of
  "trace" -> Right Trace
  "debug" -> Right Debug
  "info" -> Right Info
  "error" -> Right Error
  _ ->
    Left $
      "Invalid log level: "
        ++ s
        ++ ". Expected one of: trace, debug, info, warn, error."
  where
    toLower = Char.toLower


parseArguments :: Parser Command
parseArguments =
  hsubparser
    ( command
        "repl"
        (info (ReplCmd <$> parseReplOptions) (progDesc "Run the REPL"))
        <> metavar "repl"
        <> help "Execute the Octizys REPL."
    )
    <|> hsubparser
      ( command
          "compile"
          (info (CompileCmd <$> parseCompileOptions) (progDesc "Compile source code"))
          <> metavar "compile"
          <> help "Execute the Otizys compiler over the given paths."
      )


parseFormatterConfiguration :: Parser Configuration
parseFormatterConfiguration =
  makeConfiguration
    <$> switch
      ( long "showAstTypeVars" <> help "Displays all the type variables in the ast."
      )
    <*> option
      auto
      ( long "indentation"
          <> help "The indentation used (in spaces) by the formatter."
          <> value 2
      )
    <*> switch
      ( long "showConstraintsReasons"
          <> help
            "Constraint shows a little description at it's side."
      )


parseReplOptions :: Parser ReplOptions
parseReplOptions =
  ReplOptions
    <$> parseLevel
    <*> switch (long "showCst" <> help "Displays the Cst after parsing.")
    <*> switch
      ( long "showAst"
          <> help
            "Display the AST after inference is done and the generated constraints."
      )
    <*> parseFormatterConfiguration


parseCompileOptions :: Parser CompileOptions
parseCompileOptions =
  CompileOptions
    <$> ( NonEmpty.fromList
            <$> some
              ( argument
                  str
                  (metavar "FILE+" <> help "Source file.")
              )
        )
    <*> parseLevel
