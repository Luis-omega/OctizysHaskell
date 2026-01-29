module Octizys.PathResolution.PathIndex
  ( PathIndex
  , PathIndexError (PathIndexError')
  , makePathIndex
  , lookupLogicPath
  , lookupSystemPath
  , RootPaths
  , makeRootPaths
  ) where

import Effectful ((:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.Internal.Monad (Eff)
import Octizys.Common.LogicPath (LogicPath)
import qualified Octizys.Common.LogicPath as LogicPath
import Octizys.Common.Name (makeName)
import Octizys.Logging.Effect (Log)
import Octizys.Logging.Entry (field)
import qualified Octizys.Logging.Loggers as Log
import Prettyprinter (Pretty (pretty))

import Data.Aeson (ToJSON)
import GHC.Generics (Generic, Generically (..))


-- TODO:STUB

{- | A new type over `[FilePath]`, the stored paths are the paths provided to
| the compiler to lookup for code
-}
newtype RootPaths = RootPaths' {unRootPaths :: [FilePath]}
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically RootPaths


-- TODO:STUB
instance Pretty RootPaths where
  pretty (RootPaths' rs) = pretty rs


makeRootPaths :: [FilePath] -> RootPaths
makeRootPaths = RootPaths'


-- TODO:STUB
data PathIndexError = PathIndexError'
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically PathIndexError


instance Pretty PathIndexError where
  pretty x = pretty $ show x


-- TODO:STUB

{- | This storages the needed information to exchange between
logic paths and system paths
-}
data PathIndex = PathIndex'
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically PathIndex


instance Pretty PathIndex where
  pretty x = pretty $ show x


makePathIndex :: RootPaths -> PathIndex
makePathIndex _ = PathIndex'


-- TODO:STUB
lookupSystemPath
  :: Error PathIndexError :> e
  => Log :> e
  => PathIndex
  -> FilePath
  -> Eff e LogicPath
lookupSystemPath _ systemPath = do
  Log.trace "Search of logic path" [field "system path" systemPath]
  let maybeLogicPath = LogicPath.singleton <$> makeName "StubName"
  case maybeLogicPath of
    Just logicPath -> do
      Log.trace
        "Search of logic path ended"
        [ field "system path" systemPath
        , field "logic path" logicPath
        ]
      pure logicPath
    Nothing -> do
      Log.error
        "No logic path found"
        [field "systemPath path" systemPath]
      Log.trace
        "Search of logic path ended"
        [field "system path" systemPath]
      throwError
        PathIndexError'


-- TODO:STUB
lookupLogicPath
  :: Error PathIndexError :> e
  => PathIndex
  -> LogicPath
  -> Eff e FilePath
lookupLogicPath = undefined
