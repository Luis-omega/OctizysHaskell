{- | This module defines the items expected to be
present at the top of a file
-}
module Octizys.Cst.TopItem where

import Octizys.Common.Id (InfoId)
import Octizys.Cst.Expression (Definition)


data Module = Module'
  { definitions :: [(Definition, InfoId)]
  -- ^ The InfoId is for the colon
  , lastComments :: Maybe InfoId
  }
  deriving (Show, Eq, Ord)
