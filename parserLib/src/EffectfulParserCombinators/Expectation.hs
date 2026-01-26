module EffectfulParserCombinators.Expectation where

import Control.Arrow ((<<<))
import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import qualified Data.Set as Set
import Prettyprinter (Pretty (pretty))
import qualified Prettyprinter as Pretty

import Data.Aeson (ToJSON)
import GHC.Generics (Generic, Generically (..))


{- | Inspired by megaparsec, it represents expectation
of some input.
-}
data Expected
  = -- | A non empty string that was expected to be seen.
    ExpectedRaw (NonEmpty Char)
  | -- | A custom name for what we expected to see.
    ExpectedName (NonEmpty Char)
  | -- | Expected end of input.
    ExpectedEndOfInput
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically Expected


instance Pretty Expected where
  pretty expt =
    case expt of
      ExpectedRaw raw -> pretty raw
      ExpectedName name -> pretty name
      ExpectedEndOfInput -> pretty @String "end of input"


-- | A newtype over a set of expectation for type safe security.
newtype Expectations = Expectations' {unExpectations :: Set Expected}
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically Expectations


instance Pretty Expectations where
  pretty es =
    let asList :: [Expected] = Set.toList $ unExpectations es
     in case asList of
          [] -> pretty @String ""
          (start : end) ->
            ( Pretty.align
                <<< Pretty.fillSep
            )
              ( pretty start
                  : ( (\x -> pretty @String "," <> pretty x) <$> end
                    )
              )


insert :: Expected -> Expectations -> Expectations
insert e (Expectations' es) = Expectations' (Set.insert e es)


union :: Expectations -> Expectations -> Expectations
union (Expectations' es') (Expectations' es) =
  Expectations' (Set.union es' es)


empty :: Expectations
empty = Expectations' Set.empty


singleton :: Expected -> Expectations
singleton = Expectations' <<< Set.singleton


instance Semigroup Expectations where
  (<>) = union


instance Monoid Expectations where
  mempty = empty
