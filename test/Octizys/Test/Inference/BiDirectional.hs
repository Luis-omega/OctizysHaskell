{-# LANGUAGE DataKinds #-}

module Octizys.Test.Inference.BiDirectional where

import Data.IORef (IORef, newIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import Effectful (runEff)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Reader.Static (runReader)
import Effectful.State.Static.Local (runState)
import GHC.Stack (HasCallStack)
import qualified Octizys.Ast.Combinators as A
import qualified Octizys.Ast.Expression as Ast
import Octizys.Ast.Type
  ( InferenceVariable
  , MonoType (Variable)
  , Type
  )
import qualified Octizys.Ast.Type as Ast
import Octizys.Classes.From (from)
import Octizys.Common.Format (indentPretty, pText, renderDoc)
import qualified Octizys.Common.Format as Common
import Octizys.Common.Format.Config (defaultConfiguration)
import Octizys.Common.Id
  ( ExpressionVariableId
  , TypeVariableId
  )
import Octizys.Effects.Accumulator.Interpreter (runAccumulatorFull)
import Octizys.Effects.Console.Interpreter (runConsole)
import Octizys.Effects.IdGenerator.Interpreter (runIdGeneratorFull)
import qualified Octizys.FrontEnd.Cst.Combinators as C
import qualified Octizys.FrontEnd.Cst.Expression as Cst
import qualified Octizys.FrontEnd.Cst.Node as Cst
import Octizys.FrontEnd.Format.Expression (formatExpression)
import Octizys.Inference.BiDirectional (solveExpressionType)
import Octizys.Inference.Constraint (ConstraintId (ConstraintId'))
import Octizys.Inference.Context (Context, contextFromList, emptyContext)
import Octizys.Logging.Interpreters.Console (runLog)
import Octizys.Logging.Levels (Level (Info))
import Prettyprinter (Pretty (pretty))
import qualified Prettyprinter as Pretty
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion
  , assertFailure
  , testCase
  )


makeMonoVar :: IORef Int -> IO (MonoType InferenceVariable)
makeMonoVar counter =
  Variable <$> C.makeVariable counter


assertEqualTypes
  :: Context
  -> Cst.Expression ExpressionVariableId TypeVariableId
  -> Type Ast.TypeVariable
  -> Type Ast.TypeVariable
  -> IO ()
assertEqualTypes ctx expr t1 t2 =
  if t1 == t2
    then pure ()
    else
      assertFailure $
        Text.unpack
          ( renderDoc $
              pretty ctx
                <> Pretty.hardline
                <> pText "Expected:"
                <> indentPretty t1
                <> Pretty.hardline
                <> pText "Result:"
                <> indentPretty t2
                <> Pretty.hardline
                <> pText "Original expression:"
                <> formatExpression defaultConfiguration expr
          )


runSolver
  :: Context
  -> Cst.Expression ExpressionVariableId TypeVariableId
  -> IO (Either Text (Ast.Expression Ast.TypeVariable))
runSolver ctx expr =
  runEff $
    runConsole $
      runLog Info $
        runErrorNoCallStack $
          runReader ctx $
            runReader C.simbolContext $
              runAccumulatorAlone [] $
                runIdGeneratoAlone 0 $
                  runStateAlone (ConstraintId' 0) (solveExpressionType expr)
  where
    runStateAlone s ef = fst <$> runState s ef
    runIdGeneratoAlone s ef = fst <$> runIdGeneratorFull s ef
    runAccumulatorAlone s ef = fst <$> runAccumulatorFull s ef


tests :: TestTree
tests =
  testGroup
    "Type checking"
    [ testCase "Literal True" $ do
        let
          expr = C.bool True
        assertInfers emptyContext expr (Ast.TMono A.boolType)
    , testCase "Literal False" $ do
        let
          expr = C.bool False
        assertInfers emptyContext expr (Ast.TMono A.boolType)
    , testCase "Literal int" $ do
        let
          expr = C.int 2
        assertInfers emptyContext expr (Ast.TMono A.intType)
    , testCase "Variable in context" $ do
        counter <- newIORef 0
        evar <- C.eVar counter
        let ctx = contextFromList [(evar.name, Ast.TMono A.boolType)] []
        assertInfers ctx evar (Ast.TMono A.boolType)
    , testCase "Variable missing" $ do
        counter <- newIORef 0
        evar <- C.eVar counter
        assertFails emptyContext evar
    , testCase "Mono application" $ do
        counter <- newIORef 0
        f <- C.eVar counter
        x <- C.eVar counter
        xT <- A.tVar counter
        let ft = A.arrow [xT] A.boolType
            ctx =
              contextFromList
                [ (f.name, Ast.TMono ft)
                , (x.name, Ast.TMono xT)
                ]
                []
            expr = C.app f [x]
        assertInfers ctx expr (Ast.TMono A.boolType)
    , testCase "If condition must be bool" $ do
        let
          expr = C.eIf (C.int 1) (C.bool False) (C.bool True)
        assertFails emptyContext expr
    , testCase "If branches must have same type" $ do
        let
          expr = C.eIf (C.bool True) (C.bool False) (C.int 2)
        assertFails emptyContext expr
    ]


assertInfers
  :: HasCallStack
  => Context
  -> Cst.Expression ExpressionVariableId TypeVariableId
  -> Ast.Type Ast.TypeVariable
  -> Assertion
assertInfers context expression expected = do
  maybeResult <- runSolver context expression
  case maybeResult of
    Right result ->
      assertEqualTypes
        context
        expression
        expected
        (Ast.TMono $ Ast.getType result)
    Left msg -> do
      assertFailure $
        Text.unpack $
          renderDoc
            ( pretty context
                <> Pretty.hardline
                <> Common.pText "Can't infer a type, but a type was expected!"
                <> Pretty.hardline
                <> pretty
                  (from @(Cst.Node ExpressionVariableId TypeVariableId) expression)
                <> Pretty.hardline
                <> pretty msg
            )


assertFails
  :: HasCallStack
  => Context
  -> Cst.Expression ExpressionVariableId TypeVariableId
  -> Assertion
assertFails context expression = do
  maybeResult <- runSolver context expression
  case maybeResult of
    Right result ->
      assertFailure $
        Text.unpack $
          renderDoc
            ( pretty context
                <> Pretty.hardline
                <> Common.pText "Expected type error, but inferred: "
                <> pretty result
            )
    Left _ -> pure ()
