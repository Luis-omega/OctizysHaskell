{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octizys.Effects.FileReader.Effect
  ( FileReader (ReadFile)
  , readFile
  , FileReadError (FileNotFound, FileOtherReadError)
  ) where

import Data.Text (Text)
import Effectful (Effect)
import Effectful.TH (makeEffect)
import Prettyprinter (Pretty, pretty)
import Prelude hiding (readFile)


data FileReadError
  = FileNotFound FilePath
  | FileOtherReadError FilePath Text
  deriving (Show, Eq)


instance Pretty FileReadError where
  pretty (FileNotFound p) = pretty @Text "FileReadError: Can't find file: " <> pretty p
  pretty (FileOtherReadError p t) =
    pretty @Text "FileReadError: trying to open "
      <> pretty p
      <> ", detailed report:"
      <> pretty t


data FileReader :: Effect where
  ReadFile :: FilePath -> FileReader m (Either FileReadError Text)


$(makeEffect ''FileReader)
