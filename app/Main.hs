module Main where

import qualified Ast (hello)


main :: IO ()
main = do
  Ast.hello
