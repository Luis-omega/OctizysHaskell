{-# LANGUAGE ScopedTypeVariables #-}

module Octizys.HistoryMap
  ( HistoryMap
  , pushChanges
  , popChanges
  , lookup
  , empty
  )
where

import Control.Arrow ((<<<))
import Data.List.NonEmpty (NonEmpty ((:|)), uncons)
import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding (lookup)


data HistoryMap k v = HistoryMapC
  { innerMap :: Map k (NonEmpty v)
  , stack :: [[k]]
  }
  deriving (Show)


pushChanges
  :: forall k v
   . Ord k
  => [(k, v)]
  -> HistoryMap k v
  -> HistoryMap k v
pushChanges changes dic =
  let innerDic = innerMap dic
      newInnerMap = foldr update innerDic changes
      newStack = (fst <$> changes) : stack dic
   in HistoryMapC {innerMap = newInnerMap, stack = newStack}
  where
    update :: (k, v) -> Map k (NonEmpty v) -> Map k (NonEmpty v)
    update (key, value) = Map.insertWith (<>) key (value :| [])


popChanges :: Ord k => HistoryMap k v -> HistoryMap k v
popChanges dic =
  case stack dic of
    [] -> dic
    (lastItems : remain) ->
      let innerDic = innerMap dic
          newInnerMap = foldr updateKey innerDic lastItems
       in HistoryMapC {innerMap = newInnerMap, stack = remain}
  where
    updateKey = Map.update (snd <<< uncons)


lookup :: Ord k => k -> HistoryMap k v -> Maybe v
lookup key dic =
  case Map.lookup key (innerMap dic) of
    Just (x :| _) -> Just x
    Nothing -> Nothing


empty :: HistoryMap k v
empty = HistoryMapC Map.empty []
