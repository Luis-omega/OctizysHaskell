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
import Prettyprinter (Pretty (pretty))

import Octizys.Common.Format.Config (defaultConfiguration)
import qualified Octizys.FrontEnd.Format.Expression as FormatE
import qualified Octizys.FrontEnd.Format.Type as FormatT


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


instance (Pretty evar, Pretty tvar) => Pretty (Node evar tvar) where
  pretty (NType t) = FormatT.format defaultConfiguration t
  pretty (NParam p) = FormatE.formatParameter defaultConfiguration p
  pretty (NDef t) = FormatE.formatDefinition defaultConfiguration t
  pretty (NExp t) = FormatE.formatExpression defaultConfiguration t
