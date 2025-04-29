{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Octizys.Effects.Console.Effect (Console(PutString,ReadLine), putString, readLine) where

import Effectful (Effect)
import Effectful.TH (makeEffect)


data Console :: Effect where
  ReadLine :: Console m String
  PutString :: String -> Console m ()

$(makeEffect ''Console)
