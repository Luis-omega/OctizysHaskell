{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octizys.Effects.IdGenerator.Effect
  ( IdGenerator (GenerateId)
  , generateId
  ) where

import Effectful (Effect)
import Effectful.TH (makeEffect)
import Octizys.Common.Id (SymbolOriginInfo)


-- | Effect for generating arbitrary values of type 'a'
data IdGenerator a :: Effect where
  GenerateId :: Maybe SymbolOriginInfo -> IdGenerator a m a


$(makeEffect ''IdGenerator)
