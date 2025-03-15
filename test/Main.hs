module Main (main) where

import qualified Dummy (tests)
import Test.Hspec (hspec)


main :: IO ()
main = hspec Dummy.tests
