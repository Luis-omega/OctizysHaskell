{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Octizys.Effects.Generator.Interpreter
  ( GenerateFromInt (generateFromInt)
  , runGenerator
  , runGeneratorWith
  , runIntGenerator
  , runGeneratorFull
  , runGeneratorFullWith
  , IntGeneratorState (IntGeneratorState', nextInt)
  ) where

import Control.Arrow ((<<<))
import qualified Data.Bifunctor as Bifunctor
import Data.Coerce (coerce)
import Effectful (Eff, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Local (State, gets, put, runState)
import Octizys.Effects.Generator.Effect
  ( Generator (Generate)
  , IntGenerator (GenerateInt)
  , generateInt
  )


{- | A type class that defines how to generate a value of type
'a' from an integer.
-}
class GenerateFromInt a where
  generateFromInt :: Int -> a


instance GenerateFromInt Int where
  generateFromInt i = i


-- | State for keeping track of fresh integer counts
newtype IntGeneratorState a = IntGeneratorState'
  { nextInt :: Int
  }


-- | Runner for the `IntGenerator` effect
runIntGenerator
  :: State (IntGeneratorState a) :> es
  => Eff (IntGenerator a : es) b
  -> Eff es b
runIntGenerator = interpret $ \_ x ->
  case x of
    GenerateInt -> do
      s <- gets nextInt
      put (IntGeneratorState' (s + 1))
      pure s


-- Runner for the Generator effect
runGeneratorWith
  :: IntGenerator a
    :> es
  => (Int -> a)
  -> Eff (Generator a : es) b
  -> Eff es b
runGeneratorWith create = interpret $ \_ x ->
  case x of
    Generate ->
      create <$> generateInt


runGenerator
  :: (IntGenerator a :> es, GenerateFromInt a)
  => Eff (Generator a : es) b
  -> Eff es b
runGenerator = runGeneratorWith generateFromInt


runGeneratorFullWith
  :: Int
  -> (Int -> a)
  -> Eff
      ( Generator a
          : IntGenerator a
          : State (IntGeneratorState a)
          : es
      )
      b
  -> Eff es (b, a)
runGeneratorFullWith seed create action = do
  Bifunctor.second (create <<< nextInt)
    <$> ( runState (coerce seed)
            <<< runIntGenerator
            <<< runGeneratorWith create
        )
      action


runGeneratorFull
  :: GenerateFromInt a
  => Int
  -> Eff
      ( Generator a
          : IntGenerator a
          : State (IntGeneratorState a)
          : es
      )
      b
  -> Eff es (b, a)
runGeneratorFull seed = runGeneratorFullWith seed generateFromInt

