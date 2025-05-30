{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Octizys.Effects.IdGenerator.Interpreter
  ( IdGenerator
  , generateId
  , runIdGenerator
  , runIdGeneratorFull
  ) where

import Effectful (Eff, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Reader.Static (Reader, ask)
import Effectful.State.Static.Local (State, gets, put, runState)
import Octizys.Common.Id (GenerateIdFromInt (generateIdFromInt))
import Octizys.Common.Qualifier (Qualifier)
import Octizys.Effects.IdGenerator.Effect
  ( IdGenerator (GenerateId)
  , generateId
  )


-- | State for keeping track of fresh integer counts
newtype IdGeneratorState a = IdGeneratorState'
  { nextInt :: Int
  }


-- | Runner for the `IntGenerator` effect
runIdGenerator
  :: State (IdGeneratorState a) :> es
  => Reader Qualifier :> es
  => GenerateIdFromInt a
  => Eff (IdGenerator a : es) b
  -> Eff es b
runIdGenerator = interpret $ \_ x ->
  case x of
    GenerateId -> do
      q <- ask
      s <- gets nextInt
      put (IdGeneratorState' (s + 1))
      pure (generateIdFromInt q s)


runIdGeneratorFull
  :: GenerateIdFromInt a
  => Reader Qualifier :> es
  => Int
  -> Eff
      ( IdGenerator a
          : State (IdGeneratorState a)
          : es
      )
      b
  -> Eff es (b, IdGeneratorState a)
runIdGeneratorFull seed action =
  runState (IdGeneratorState' seed) $
    runIdGenerator action
