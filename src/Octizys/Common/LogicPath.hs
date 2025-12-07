module Octizys.Common.LogicPath
  ( LogicPath
  , makeLogicPath
  , addAtEnd
  , singleton
  ) where

import Data.List.NonEmpty (NonEmpty, toList)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import GHC.Base (NonEmpty ((:|)))
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


instance From (NonEmpty Name) LogicPath where
  from = logicPathRaw


instance Pretty LogicPath where
  pretty (LogicPath' names) =
    case names of
      onlyName :| [] -> pretty onlyName
      other ->
        let withSeparator =
              ((\x -> pretty x <> pretty '/') <$> other)
         in foldl (<>) mempty withSeparator


-- | Adds the given name at the end of a logic path.
addAtEnd :: Name -> LogicPath -> LogicPath
addAtEnd name p =
  LogicPath'
    ( NonEmpty.appendList
        p.logicPathRaw
        [name]
    )


singleton :: Name -> LogicPath
singleton name = LogicPath' (name NonEmpty.:| [])
