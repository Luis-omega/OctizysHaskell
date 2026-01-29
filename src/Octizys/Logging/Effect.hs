{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octizys.Logging.Effect where

import Effectful (Effect)
import Effectful.TH (makeEffect)
import Octizys.Logging.Entry (Entry)


data Log :: Effect where
  LogEntry :: Entry -> Log m ()


$(makeEffect ''Log)
