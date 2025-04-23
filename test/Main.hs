module Main (main) where

import Test.Hspec (hspec)
import qualified Test.Parser.PrettyParse as Parser


main :: IO ()
main = do
  hspec Parser.tests
