module Octizys.Common.LogicPath (LogicPath, makeLogicPath) where

import Data.List.NonEmpty (NonEmpty, toList)
import Data.Text (Text)
import Octizys.Common.Name (Name)
import Prettyprinter (Pretty (pretty))
import qualified Prettyprinter as Pretty


-- | Represents a module direction.
newtype LogicPath = LogicPath' {logicPathRaw :: NonEmpty Name}
  deriving (Show, Eq, Ord)


makeLogicPath :: Text -> LogicPath
makeLogicPath = undefined


instance Pretty LogicPath where
  pretty lp =
    let withSeparator =
          ((\x -> pretty x <> pretty '/') <$> toList lp.logicPathRaw)
     in foldl (<>) mempty withSeparator

