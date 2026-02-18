{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Octizys.FrontEnd.Cst.Node where

import Octizys.Classes.From (From (from))
import Octizys.FrontEnd.Cst.Expression
  ( Definition
  , Expression
  , Parameter
  )
import Octizys.FrontEnd.Cst.Type (Type)

import Data.Aeson (ToJSON)
import GHC.Generics (Generic, Generically (..))
import Octizys.Format.Class (Formattable (format))


data Node evar tvar
  = NType (Type tvar)
  | NParam (Parameter evar tvar)
  | NDef (Definition evar tvar)
  | NExp (Expression evar tvar)
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (Node evar tvar)


instance From (Node evar tvar) (Type tvar) where
  from = NType


instance From (Node evar tvar) (Parameter evar tvar) where
  from = NParam


instance From (Node evar tvar) (Definition evar tvar) where
  from = NDef


instance From (Node evar tvar) (Expression evar tvar) where
  from = NExp


instance (Formattable evar, Formattable tvar) => Formattable (Node evar tvar) where
  format configuration (NType t) = format configuration t
  format configuration (NParam p) = format configuration p
  format configuration (NDef t) = format configuration t
  format configuration (NExp t) = format configuration t
