{- | This module defines the items expected to be
present at the top of a file
-}
module Octizys.Cst.TopItem where

import Octizys.Cst.Expression (Definition)
import Octizys.Cst.SourceInfo (SourceInfo)


data Module evar tvar = Module'
  { definitions :: [(Definition evar tvar, SourceInfo)]
  -- ^ The InfoId is for the colon
  , lastComments :: Maybe SourceInfo
  }
  deriving (Show, Eq, Ord)
