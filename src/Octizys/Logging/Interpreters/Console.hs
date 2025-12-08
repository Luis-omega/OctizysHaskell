{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Octizys.Logging.Interpreters.Console
  ( runLog
  ) where

import Control.Arrow ((<<<))
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as Text
import Effectful (Eff, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Octizys.Effects.Console.Effect (Console)
import Octizys.Effects.Console.Interpreter (putLine)
import Octizys.Logging.Effect (Log (LogEntry))
import Octizys.Logging.Entry (getFields, getLevel, getMessage)
import Octizys.Logging.Levels (Level)
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


inQuotes :: Text -> Doc ann
inQuotes txt =
  pretty '"'
    <> pretty txt
    <> pretty '"'


prettyJSONField :: Text -> Text -> Doc ann
prettyJSONField name value =
  inQuotes name
    <> pretty ':'
    <> inQuotes value


runLog
  :: Console :> es
  => Level
  -> Eff (Log ': es) a
  -> Eff es a
runLog minLevel = interpret $ \_ action ->
  case action of
    LogEntry entry ->
      let
        level = getLevel entry
        msg = getMessage entry
        fields = getFields entry
       in
        when (minLevel <= level) $
          putLine $
            render
              ( pretty '{'
                  <> prettyJSONField "level" (Text.pack $ show level)
                  <> pretty ','
                  <> prettyJSONField "message" msg
                  <> pretty ','
                  <> prettyJSONField "fields" (render $ pretty fields)
                  <> pretty '}'
              )
