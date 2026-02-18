{-# LANGUAGE DataKinds #-}

module Octizys.Effects.Accumulator.Utils
  ( accumulateErrors
  , throwAccumulator
  ) where

import Control.Monad (foldM)
import Effectful ((:>))
import Effectful.Error.Static
  ( Error
  , runErrorNoCallStack
  , throwError
  )
import Effectful.Internal.Monad (Eff)
import Effectful.State.Static.Local (State)
import Octizys.Effects.Accumulator.Effect (Accumulator, accumulate)
import Octizys.Effects.Accumulator.Interpreter
  ( AccumulatorState
  , runAccumulatorFull
  )


accumulateErrors
  :: forall a b acc es
   . Accumulator acc :> es
  => (a -> Eff (Error acc : es) b)
  -> [a]
  -> Eff es [b]
accumulateErrors f actions = do
  foldM
    ( \acc new -> do
        result <- runErrorNoCallStack (f new)
        case result of
          Left e -> accumulate e >> pure acc
          Right v -> pure (v : acc)
    )
    []
    actions


throwAccumulator
  :: Error [acc] :> es
  => Eff (Accumulator acc : State (AccumulatorState acc) : es) a
  -> Eff es a
throwAccumulator action = do
  (_, errors) <- runAccumulatorFull [] action
  throwError errors
