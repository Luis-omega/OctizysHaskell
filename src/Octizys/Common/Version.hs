module Octizys.Common.Version
  ( Version
  , getMajor
  , getMinor
  , getPatch
  , makeVersion
  ) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic, Generically (..))
import Prettyprinter (Pretty (pretty))


-- | A simplificated representation of Semanting version
data Version = Version'
  { major :: Int
  , minor :: Int
  , patch :: Int
  }
  deriving (Eq, Ord, Show, Generic)
  deriving (ToJSON) via Generically Version


getMajor :: Version -> Int
getMajor = major


getMinor :: Version -> Int
getMinor = minor


getPatch :: Version -> Int
getPatch = patch


makeVersion :: Int -> Int -> Int -> Version
makeVersion = Version'


instance Pretty Version where
  pretty v =
    pretty v.major
      <> pretty '.'
      <> pretty v.minor
      <> pretty '.'
      <> pretty v.patch
