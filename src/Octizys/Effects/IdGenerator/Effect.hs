{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Octizys.Effects.IdGenerator.Effect
  ( IdGenerator (GenerateId)
  , generateId
  ) where

import Effectful (Effect)
import Effectful.TH (makeEffect)


-- | Effect for generating arbitrary values of type 'a'
data IdGenerator a :: Effect where
  GenerateId :: IdGenerator a m a


$(makeEffect ''IdGenerator)
