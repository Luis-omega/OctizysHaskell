module Main where

import qualified Random (someFunc)


main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
