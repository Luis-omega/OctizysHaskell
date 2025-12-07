{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Octizys.Effects.Logger.ConsoleInterpreter
  ( runLog
  ) where

import Control.Arrow ((<<<))
import Control.Monad (when)
import Data.Text (Text)
import Effectful (Eff, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Octizys.Effects.Console.Effect (Console)
import Octizys.Effects.Console.Interpreter (putLine)
import Octizys.Effects.Logger.Effect
  ( LogEntry (LogEntry', fileName, line, logLevel, message)
  , LogLevel
  , Logger (LogMessage)
  )
import Prettyprinter
  ( Doc
  , Pretty (pretty)
  , defaultLayoutOptions
  , layoutPretty
  )
import Prettyprinter.Render.Text (renderStrict)


render :: Doc ann -> Text
render =
  renderStrict
    <<< layoutPretty defaultLayoutOptions


runLog
  :: Console :> es
  => LogLevel
  -> Eff (Logger ': es) a
  -> Eff es a
runLog minLevel = interpret $ \_ action ->
  case action of
    LogMessage
      ( LogEntry'
          { message = msg
          , logLevel = lvl
          , fileName
          , line
          }
        ) ->
        when (minLevel <= lvl) $
          putLine
            ( render
                ( pretty '['
                    <> pretty lvl
                    <> pretty @Text "]["
                    <> pretty fileName
                    <> pretty @Text "]["
                    <> pretty line
                    <> pretty ']'
                )
                <> msg
            )
