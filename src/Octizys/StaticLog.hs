{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Octizys.StaticLog
  ( logError
  , logWarn
  , logInfo
  , logDebug
  , logTrace
  , buildLog
  ) where

import Effectful
import Effectful.Dispatch.Dynamic (send)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift, location)
import Prettyprinter

import Octizys.Effects.Logger.Effect
  ( LogEntry (LogEntry')
  , LogLevel (Debug, Error, Info, Trace, Warn)
  , logMessage
  )

import Control.Arrow ((<<<))
import Data.Text (Text)
import qualified Data.Text as Text


logWithLoc :: LogLevel -> Q Exp
logWithLoc lvl = do
  loc <- location
  let file = Text.pack $ loc_module loc
      (lineNum, _) = loc_start loc
  [|\msg -> logMessage (LogEntry' lvl file lineNum msg)|]


logError = logWithLoc Error
logWarn = logWithLoc Warn
logInfo = logWithLoc Info
logDebug = logWithLoc Debug
logTrace = logWithLoc Trace


class BuildLogText r where
  buildLog :: Doc () -> r


instance BuildLogText (Doc ()) where
  buildLog t = t


instance (Pretty a, BuildLogText r) => BuildLogText (a -> r) where
  buildLog f = \at -> buildLog (f <> pretty at)
