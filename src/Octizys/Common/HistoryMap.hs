module Octizys.Common.HistoryMap
  ( HistoryMap
  , pushValue
  , popValue
  , lookup
  , empty
  )
where

import Control.Arrow ((<<<))
import Data.List.NonEmpty (NonEmpty ((:|)), uncons)
import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding (lookup)


newtype HistoryMap k v = HistoryMap'
  { innerMap :: Map k (NonEmpty v)
  }
  deriving (Show)


pushValue
  :: forall k v
   . Ord k
  => (k, v)
  -> HistoryMap k v
  -> HistoryMap k v
pushValue (key, value) dic =
  HistoryMap'
    ( Map.insertWith
        (<>)
        key
        (value :| [])
        (innerMap dic)
    )


popValue :: Ord k => k -> HistoryMap k v -> HistoryMap k v
popValue key dic =
  HistoryMap'
    ( Map.update
        (snd <<< uncons)
        key
        (innerMap dic)
    )


lookup :: Ord k => k -> HistoryMap k v -> Maybe v
lookup key dic =
  case Map.lookup key (innerMap dic) of
    Just (x :| _) -> Just x
    Nothing -> Nothing


empty :: HistoryMap k v
empty = HistoryMap' Map.empty
