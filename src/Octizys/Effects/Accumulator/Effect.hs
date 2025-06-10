{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octizys.Effects.Accumulator.Effect where

import Effectful (Effect)
import Effectful.TH (makeEffect)


data Accumulator a :: Effect where
  Accumulate :: a -> Accumulator a m ()


$(makeEffect ''Accumulator)
