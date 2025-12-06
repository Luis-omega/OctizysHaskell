module Main (main) where

import qualified Octizys.Test.Parser.PrettyParse as PrettyParse
import Test.Tasty (defaultMain, testGroup)


main :: IO ()
main =
  defaultMain $
    testGroup
      ""
      [ PrettyParse.tests
      ]
