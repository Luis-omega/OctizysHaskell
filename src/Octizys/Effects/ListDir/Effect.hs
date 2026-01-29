{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octizys.Effects.ListDir.Effect where

import Data.Text (Text)
import Effectful (Effect)
import Effectful.TH (makeEffect)
import Prettyprinter (Pretty, pretty)
import Prelude hiding (readFile)


data ListDirError
  = DirNotFound FilePath
  | DirOtherReadError FilePath Text
  deriving (Show, Eq)


instance Pretty ListDirError where
  pretty (DirNotFound p) = pretty @Text "ListDirError: Can't find path: " <> pretty p
  pretty (DirOtherReadError p t) =
    pretty @Text "ListDirError: trying to list "
      <> pretty p
      <> ", detailed report:"
      <> pretty t


data ListDir :: Effect where
  ListDir :: FilePath -> ListDir m (Either ListDirError [FilePath])


$(makeEffect ''ListDir)
