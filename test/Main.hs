module Main (main) where

import Octizys.Test.Parser.PrettyParse qualified as Parser
import Test.Hspec (hspec)


main :: IO ()
main = do
  hspec Parser.tests
