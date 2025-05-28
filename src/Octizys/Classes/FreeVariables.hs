{-# LANGUAGE MultiParamTypeClasses #-}

module Octizys.Classes.FreeVariables where

import Data.Set (Set)


class Ord var => FreeVariables var t where
  freeVariables :: t -> Set var
