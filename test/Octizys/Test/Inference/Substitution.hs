module Octizys.Test.Inference.Substitution where

import Control.Arrow ((<<<))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Octizys.Ast.Type
  ( InferenceVariable
  , MonoType (Variable)
  , TypeValue (BoolType, IntType)
  )
import Octizys.Classes.From (from)
import Octizys.Common.Format (indentPretty, pText, renderDoc)
import Octizys.Common.Id
  ( SymbolContext (SymbolContext')
  , TypeVariableId
  , generateFromInt
  )
import Octizys.Common.LogicPath (LogicPath)
import qualified Octizys.Common.LogicPath as LogicPath
import Octizys.Common.Name (Name, makeName)
import Octizys.Common.Qualifier (Qualifier)
import qualified Octizys.Inference.Substitution as Substitution
import qualified Octizys.Package.Reference as Package
import qualified Prettyprinter as Pretty
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))


pRef :: Package.Reference
pRef =
  Package.TargetPackage $
    Package.makeOptionalIdentity
      Nothing
      Nothing
      (Package.makeSource "test")


unsafeMakeName :: Text -> Name
unsafeMakeName = fromJust <<< makeName


sc :: SymbolContext
sc =
  let
    name = unsafeMakeName "Test"
    path :: LogicPath = LogicPath.singleton name
    qualifier :: Qualifier = from path
   in
    SymbolContext' pRef qualifier


makeTypeVariableId :: IORef Int -> IO TypeVariableId
makeTypeVariableId varCounter = do
  currentValue <- readIORef varCounter
  let
    tVar = generateFromInt sc Nothing currentValue
  writeIORef varCounter (currentValue + 1)
  pure tVar


makeMonoVar :: IORef Int -> IO (MonoType InferenceVariable)
makeMonoVar counter =
  (Variable <<< from) <$> makeTypeVariableId counter


assertEqualTypes
  :: MonoType InferenceVariable
  -> MonoType InferenceVariable
  -> IO ()
assertEqualTypes t1 t2 =
  if t1 == t2
    then pure ()
    else
      assertFailure $
        Text.unpack
          ( renderDoc $
              pText "Expected:"
                <> indentPretty t1
                <> Pretty.line
                <> pText "Result:"
                <> indentPretty t2
          )


tests :: TestTree
tests =
  testGroup
    "Substitution"
    [ testCase "ignores type value" $ do
        counter <- newIORef 0
        var <- makeTypeVariableId counter
        let
          ty1 = from BoolType
          ty2 = from IntType
          s = Substitution.singleton var ty2
        ty1 @?= Substitution.applyToMonoType s ty1
    , testCase "replaces var" $ do
        counter <- newIORef 0
        var <- makeTypeVariableId counter
        let
          ty1 :: MonoType InferenceVariable = Variable (from var)
          ty2 = from IntType
          s = Substitution.singleton var ty2
        ty2 @?= Substitution.applyToMonoType s ty1
    , testCase "ignore other var" $ do
        counter <- newIORef 0
        var1 <- makeTypeVariableId counter
        var2 <- makeTypeVariableId counter
        let
          ty1 :: MonoType InferenceVariable = Variable (from var1)
          ty2 = from BoolType
          s = Substitution.singleton var2 ty2
        assertEqualTypes ty1 (Substitution.applyToMonoType s ty1)
    ]
