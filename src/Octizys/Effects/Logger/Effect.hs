{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octizys.Effects.Logger.Effect where

import Control.Arrow ((<<<))
import Data.Text (Text)
import Effectful (Eff, Effect, (:>))
import Effectful.TH (makeEffect)
import Language.Haskell.TH.Syntax (Lift)
import Prettyprinter (Doc, Pretty (pretty))


data LogLevel = Error | Debug | Info | Trace | Warn
  deriving (Show, Eq, Lift)


instance Ord LogLevel where
  _ <= Error = True
  Error <= _ = False
  _ <= Warn = True
  Warn <= _ = False
  _ <= Info = True
  Info <= _ = False
  _ <= Debug = True
  Debug <= _ = False
  Trace <= _ = True


instance Pretty LogLevel where
  pretty = pretty <<< show


data LogEntry = LogEntry'
  { logLevel :: LogLevel
  , fileName :: Text
  , line :: Int
  , message :: Text
  }
  deriving (Eq, Ord, Show, Lift)


data Logger :: Effect where
  LogMessage :: LogEntry -> Logger m ()


$(makeEffect ''Logger)
