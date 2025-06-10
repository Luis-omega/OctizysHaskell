module Octizys.Common.LogicPath (LogicPath, makeLogicPath) where

import Data.List.NonEmpty (NonEmpty, toList)
import Data.Text (Text)
import Octizys.Classes.From (From (from))
import Octizys.Common.Name (Name)
import Prettyprinter (Pretty (pretty))


-- | Represents a module direction.
newtype LogicPath = LogicPath'
  { logicPathRaw :: NonEmpty Name
  }
  deriving (Show, Eq, Ord)


makeLogicPath :: Text -> LogicPath
makeLogicPath = undefined


instance From LogicPath (NonEmpty Name) where
  from = LogicPath'


instance Pretty LogicPath where
  pretty lp =
    let withSeparator =
          ((\x -> pretty x <> pretty '/') <$> toList lp.logicPathRaw)
     in pretty '/' <> foldl (<>) mempty withSeparator
