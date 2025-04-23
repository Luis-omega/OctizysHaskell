{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Repl.Console (Console, putLine, putString, readLine, runConsole) where

import Effectful (Eff, Effect, IOE, MonadIO (liftIO), (:>))
import Effectful.Dispatch.Dynamic (HasCallStack, interpret)
import Effectful.TH (makeEffect)
import System.IO (hFlush, stdout)


data Console :: Effect where
  ReadLine :: Console m String
  PutString :: String -> Console m ()


$(makeEffect ''Console)


putLine :: Console :> es => String -> Eff es ()
putLine s = putString (s <> "\n")


runConsole :: (HasCallStack, IOE :> es) => Eff (Console : es) a -> Eff es a
-- TODO: Add IO error handling
runConsole = interpret $ \_ x -> case x of
  ReadLine -> liftIO getLine
  PutString value -> liftIO $ do
    putStr value
    hFlush stdout
