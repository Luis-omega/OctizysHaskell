module Octizys.Common.Name (Name, makeName) where

import Control.Arrow ((<<<))
import Data.Aeson (ToJSON, ToJSONKey)
import Data.Text (Text)
import GHC.Generics (Generic, Generically (..))
import Prettyprinter (Pretty (pretty))


{- | The text must be a string without spaces or line breaks.
This is the underlying type used to capture variable names,
so it should follow the same rules as them.
-}
newtype Name = Name' {nameRaw :: Text}
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically Name


instance ToJSONKey Name


-- TODO: Check invariants
makeName :: Text -> Maybe Name
makeName = Just <<< Name'


instance Pretty Name where
  pretty = pretty <<< nameRaw
