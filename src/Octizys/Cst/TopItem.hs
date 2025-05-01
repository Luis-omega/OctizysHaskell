{- | This module defines the items expected to be
present at the top of a file
-}
module Octizys.Cst.TopItem where

import Octizys.Cst.Expression (Definition)
import Octizys.Cst.InfoId (InfoId)


data Module = Module'
  { definitions :: [(Definition, InfoId)]
  -- ^ The InfoId is for the colon
  , lastComments :: Maybe InfoId
  }
  deriving (Show, Eq, Ord)
