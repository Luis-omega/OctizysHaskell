module Octizys.Compiler.Warn where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic, Generically (..))
import Prettyprinter (Pretty (pretty))


-- TODO:STUB
data Warn = Warn'
  deriving (Show, Eq, Generic)
  deriving (ToJSON) via Generically Warn


-- TODO:STUB
instance Pretty Warn where
  pretty Warn' = pretty @Text "WarnStub"
