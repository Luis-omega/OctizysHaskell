module Dummy (tests) where

import Test.Hspec


tests :: SpecWith ()
tests = do
  describe "Dummy tests" $ do
    it "compare 1 and 1" $ do
      1 `shouldBe` (1 :: Int)
