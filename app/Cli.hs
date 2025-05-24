module Cli
  ( Command (..)
  , ReplOptions (..)
  , CompileOptions (..)
  , parseArguments
  ) where

import qualified Data.Char as Char
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Octizys.Effects.Logger.Effect (LogLevel (Debug, Error, Info))
import Options.Applicative


data Command
  = ReplCmd ReplOptions
  | CompileCmd CompileOptions
  deriving (Show)


data ReplOptions = ReplOptions
  { logLevel :: LogLevel
  , showCst :: Bool
  , showInference :: Bool
  }
  deriving (Show)


data CompileOptions = CompileOptions
  { files :: NonEmpty FilePath
  , logLevel :: LogLevel
  }
  deriving (Show)


parseLogLevel :: Parser LogLevel
parseLogLevel =
  option
    (eitherReader readLogLevel)
    ( long "logLevel"
        <> metavar "LOG_LEVEL"
        <> value Info
        <> help "Set log level: debug, info, error (default: info)"
    )


readLogLevel :: String -> Either String LogLevel
readLogLevel s = case map toLower s of
  "debug" -> Right Debug
  "info" -> Right Info
  "error" -> Right Error
  _ ->
    Left $
      "Invalid log level: "
        ++ s
        ++ ". Expected one of: debug, info, warn, error."
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


parseReplOptions :: Parser ReplOptions
parseReplOptions =
  ReplOptions
    <$> parseLogLevel
    <*> switch (long "showCst" <> help "Displays the Cst after parsing.")
    <*> switch
      ( long "showAst"
          <> help
            "Display the AST after inference is done and the generated constraints."
      )


parseCompileOptions :: Parser CompileOptions
parseCompileOptions =
  CompileOptions
    <$> ( NonEmpty.fromList
            <$> some
              ( argument
                  str
                  (metavar "FILE" <> help "Source file.")
              )
        )
    <*> parseLogLevel
