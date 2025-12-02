module Main (main) where

import qualified EffectfulParserCombinators.Test.Parser as Parser
import Test.Tasty (defaultMain, testGroup)


main :: IO ()
main =
  defaultMain $
    testGroup
      ""
      [ Parser.tests
      ]
