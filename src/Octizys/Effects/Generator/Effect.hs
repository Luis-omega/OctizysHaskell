{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Octizys.Effects.Generator.Effect
  ( Generator (Generate)
  , generate
  , IntGenerator (GenerateInt)
  , generateInt
  ) where

import Effectful (Effect)
import Effectful.TH (makeEffect)


-- | Effect for generating fresh integers with phantom types
data IntGenerator a :: Effect where
  GenerateInt :: IntGenerator a m Int


$(makeEffect ''IntGenerator)


-- | Effect for generating arbitrary values of type 'a'
data Generator a :: Effect where
  Generate :: Generator a m a


$(makeEffect ''Generator)
