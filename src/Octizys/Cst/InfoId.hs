{- | This module defines a Id type to be used in
all elements of Cst and Ast whenever a reference to a source position
or commentaries or other annotations on the ast are needed.

All elements of the CST have a phantom type signaling the kind
of information they carry.

This way we can discart away comentaries for inference
or source position for evaluation.
-}
module Octizys.Cst.InfoId
  ( InfoId (InfoId', unInfoId)
  ) where

import Control.Arrow ((<<<))
import Octizys.Effects.Generator.Interpreter (GenerateFromInt)
import Prettyprinter (Pretty (pretty))


{- | A Id that signals some information of a node that
is not stored in the Cst
-}
newtype InfoId = InfoId' {unInfoId :: Int}
  deriving (Show, Eq, Ord, GenerateFromInt) via Int


instance Pretty InfoId where
  pretty = pretty <<< unInfoId
