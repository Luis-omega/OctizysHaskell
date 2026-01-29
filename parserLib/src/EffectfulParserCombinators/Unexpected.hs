module EffectfulParserCombinators.Unexpected where

import Data.List.NonEmpty (NonEmpty)
import Prettyprinter (Pretty (pretty))

import Data.Aeson (ToJSON (toJSON))
import GHC.Generics (Generic, Generically (..))


-- | Represent a unexpected element in the stream.
data Unexpected
  = -- | The original chain of characters.
    UnexpectedRaw (NonEmpty Char)
  | -- | Reached end of input
    UnexpectedEndOfInput
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically Unexpected


instance Pretty Unexpected where
  pretty expt =
    case expt of
      UnexpectedRaw raw -> pretty raw
      UnexpectedEndOfInput -> pretty @String "end of input"

-- instance Formatter ann (FormatContext ann) Unexpected where
--   format _ expt = pretty expt
