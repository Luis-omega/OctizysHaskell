{-# LANGUAGE MultiParamTypeClasses #-}

module Octizys.Classes.From (From (from)) where


class From a b where
  from :: b -> a
