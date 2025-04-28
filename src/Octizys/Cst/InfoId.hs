{- | This module defines a Id type to be used in
all elements of Cst and Ast whenever a reference to a source position
or commentaries or other annotations on the ast are needed.

All elements of the CST have a phantom type signaling the kind
of information they carry.

This way we can discart away comentaries for inference
or source position for evaluation.
-}
module Octizys.Cst.InfoId (InfoId, freshInfoId) where

import Effectful (Eff, (:>))
import Octizys.Effects.Generator.Effect
  ( Generator
  , generate
  )
import Octizys.Effects.Generator.Interpreter (GenerateFromInt)


{- | A Id that signals some information of a node that
is not stored in the Cst
-}
newtype InfoId = InfoIdC Int
  deriving (Show, Eq, Ord, GenerateFromInt) via Int


{- | Generates a new fresh InfoId used to store information in
a different structure than the Cst.
All Ids are guaranteed to be unique.
-}
freshInfoId
  :: Generator InfoId :> es => Eff es InfoId
freshInfoId = generate
