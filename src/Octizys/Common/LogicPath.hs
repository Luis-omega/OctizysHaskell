module Octizys.Common.LogicPath (LogicPath, makeLogicPath) where

import Data.List.NonEmpty (NonEmpty, toList)
import Data.Text (Text)
import Octizys.Common.Name (Name)
import Octizys.Common.PackageName (PackageName)
import Prettyprinter (Pretty (pretty))


-- | Represents a module direction.
data LogicPath = LogicPath'
  { packageName :: PackageName
  , logicPathRaw :: NonEmpty Name
  }
  deriving (Show, Eq, Ord)


makeLogicPath :: Text -> LogicPath
makeLogicPath = undefined


instance Pretty LogicPath where
  pretty lp =
    let withSeparator =
          ((\x -> pretty x <> pretty '/') <$> toList lp.logicPathRaw)
     in pretty lp.packageName <> pretty '/' <> foldl (<>) mempty withSeparator
