{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Octizys.Effects.Console.Interpreter (putLine, runConsole) where

import Effectful (Eff, IOE, MonadIO (liftIO), (:>))
import Effectful.Dispatch.Dynamic (HasCallStack, interpret)
import Octizys.Effects.Console.Effect
  ( Console (PutString, ReadLine)
  , putString
  )
import System.IO (hFlush, stdout)


putLine :: Console :> es => String -> Eff es ()
putLine s = putString (s <> "\n")


runConsole :: (HasCallStack, IOE :> es) => Eff (Console : es) a -> Eff es a
-- TODO: Add IO error handling
runConsole = interpret $ \_ x -> case x of
  ReadLine -> liftIO getLine
  PutString value -> liftIO $ do
    putStr value
    hFlush stdout
