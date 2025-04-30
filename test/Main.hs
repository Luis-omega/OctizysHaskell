module Main (main) where

-- import Octizys.Test.Parser.PrettyParse qualified as Parser
import qualified Octizys.Test.Effects.Parser as Effects.Parser
import qualified Octizys.Test.Parser.PrettyParse as PrettyParse
import Test.Hspec (hspec)


main :: IO ()
main = do
  hspec $ do
    Effects.Parser.tests
    PrettyParse.tests
