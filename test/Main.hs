module Main (main) where

import qualified Octizys.Test.Parser.PrettyParse as Parser
import Test.Hspec (hspec)


main :: IO ()
main = do
  hspec Parser.tests
