{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Octizys.Effects.FileReader.Interpreter (runFileReader) where

import Control.Exception (IOException, try)
import Data.Bifunctor (Bifunctor (first))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Effectful (Eff, IOE, MonadIO (liftIO), (:>))
import Effectful.Dispatch.Dynamic (HasCallStack, interpret)
import Octizys.Effects.FileReader.Effect
  ( FileReadError (FileOtherReadError)
  , FileReader (ReadFile)
  )


runFileReader
  :: (HasCallStack, IOE :> es)
  => Eff (FileReader : es) a
  -> Eff es a
runFileReader = interpret $ \_ x -> case x of
  ReadFile path ->
    liftIO
      ( first (\e -> FileOtherReadError path (Text.pack $ show e))
          <$> try @IOError (Text.readFile path)
      )
