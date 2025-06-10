{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Octizys.Effects.Accumulator.Interpreter
  ( runAccumulator
  , runAccumulatorFull
  , Accumulator
  , accumulate
  ) where

import Effectful (Eff, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Local (State, modify, runState)
import GHC.Stack (HasCallStack)
import Octizys.Effects.Accumulator.Effect
  ( Accumulator (Accumulate)
  , accumulate
  )


-- TODO: use sequence?
newtype AccumulatorState a = AccumulatorState'
  { unAccumulator :: [a]
  }


runAccumulator
  :: HasCallStack
  => State (AccumulatorState acc) :> es
  => Eff (Accumulator acc : es) a
  -> Eff es a
runAccumulator = interpret $ \_ x -> case x of
  Accumulate newItem ->
    modify
      ( \s ->
          s
            { unAccumulator =
                newItem : s.unAccumulator
            }
      )


runAccumulatorFull
  :: forall es a acc
   . HasCallStack
  => [acc]
  -> Eff (Accumulator acc : State (AccumulatorState acc) : es) a
  -> Eff es (a, [acc])
runAccumulatorFull initialState action =
  let afterAcc :: Eff (State (AccumulatorState acc) : es) a =
        runAccumulator action
      st :: AccumulatorState acc =
        AccumulatorState' initialState
   in (\(a, x) -> (a, x.unAccumulator))
        <$> runState
          st
          afterAcc
