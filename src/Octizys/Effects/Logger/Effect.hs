{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Octizys.Effects.Logger.Effect where

import Control.Arrow ((<<<))
import Effectful (Eff, Effect, (:>))
import Effectful.TH (makeEffect)
import Prettyprinter (Doc, Pretty (pretty))


data LogLevel = Error | Debug | Info | Trace
  deriving (Show, Eq)


instance Ord LogLevel where
  _ <= Error = True
  Error <= _ = False
  _ <= Info = True
  Info <= _ = False
  _ <= Debug = True
  Debug <= _ = False
  Trace <= _ = True


instance Pretty LogLevel where
  pretty = pretty <<< show


data Logger :: Effect where
  LogMessage :: LogLevel -> Doc ann -> Logger m ()


$(makeEffect ''Logger)


errorLog
  :: Logger :> es
  => Doc ann
  -> Eff es ()
errorLog = logMessage Error


debug
  :: Logger :> es
  => Doc ann
  -> Eff es ()
debug = logMessage Debug


info
  :: Logger :> es
  => Doc ann
  -> Eff es ()
info = logMessage Info


traceLog
  :: Logger :> es
  => Doc ann
  -> Eff es ()
traceLog = logMessage Trace
