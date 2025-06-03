{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Octizys.Cst.Node where

import Octizys.Classes.From (From (from))
import Octizys.Cst.Expression
  ( Definition
  , Expression
  , Parameter
  )
import Octizys.Cst.Type (Type)


data Node evar tvar
  = NType (Type tvar)
  | NParam (Parameter evar tvar)
  | NDef (Definition evar tvar)
  | NExp (Expression evar tvar)
  deriving (Show, Eq, Ord)


instance From (Node evar tvar) (Type tvar) where
  from = NType


instance From (Node evar tvar) (Parameter evar tvar) where
  from = NParam


instance From (Node evar tvar) (Definition evar tvar) where
  from = NDef


instance From (Node evar tvar) (Expression evar tvar) where
  from = NExp
