module Octizys.Package.Index
  ( Index
  , empty
  , build
  , listModules
  ) where

import Data.Aeson (ToJSON)
import Effectful (Eff, (:>))
import GHC.Generics (Generic, Generically (..))
import Octizys.Logging.Effect (Log)
import qualified Octizys.Module.Index as Module


-- TODO:STUB
data Index = Index'
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically Index


empty :: Index
empty = Index'


-- TODO:STUB
build
  :: Log :> e
  => FilePath
  -- ^ A file path or folder path, in case both of them are valid (a file and folder sharing name) both of them will be used for the construction of the @Index@
  -> Eff e Index
build _ = pure Index'


-- TODO:STUB
listModules :: Index -> [Module.Path]
listModules _ = []
