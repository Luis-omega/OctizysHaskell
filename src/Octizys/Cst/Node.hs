{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Octizys.Cst.Node where

import Octizys.Classes.From (From (from))
import Octizys.Cst.Expression
  ( Definition
  , Expression
  , Function
  , Parameter
  )
import Octizys.Cst.Type (Type)


data Node
  = NType Type
  | NParam Parameter
  | NDef Definition
  | NFunction Function
  | NExp Expression
  deriving (Show, Eq, Ord)


instance From Node Type where
  from = NType


instance From Node Parameter where
  from = NParam


instance From Node Definition where
  from = NDef


instance From Node Function where
  from = NFunction


instance From Node Expression where
  from = NExp
