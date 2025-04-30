{- | This module defines a Id type to be used in
all elements of Cst and Ast whenever a reference to a source position
or commentaries or other annotations on the ast are needed.

All elements of the CST have a phantom type signaling the kind
of information they carry.

This way we can discart away comentaries for inference
or source position for evaluation.
-}
module Octizys.Cst.InfoId (InfoId(unInfoId), freshInfoId) where

import Effectful (Eff, (:>))
import Octizys.Effects.Generator.Effect
  ( Generator
  , generate
  )
import Octizys.Effects.Generator.Interpreter (GenerateFromInt)
import Prettyprinter (Pretty (pretty))
import Control.Arrow ((<<<))


{- | A Id that signals some information of a node that
is not stored in the Cst
-}
newtype InfoId = InfoId' {unInfoId:: Int}
  deriving (Show, Eq, Ord, GenerateFromInt) via Int

instance Pretty InfoId where
  pretty = pretty <<< unInfoId

{- | Generates a new fresh InfoId used to store information in
a different structure than the Cst.
All Ids are guaranteed to be unique.
-}
freshInfoId
  :: Generator InfoId :> es => Eff es InfoId
freshInfoId = generate
