{-# LANGUAGE DataKinds #-}

module Octizys.Test.Inference.BiDirectional where

import Data.IORef (IORef, newIORef)
import Data.Map (Map)
import qualified Data.Map as Map
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
  , TypeEq (typeEq)
  )
import qualified Octizys.Ast.Type as Ast
import Octizys.Classes.From (from)
import Octizys.Common.Format (indentDoc, indentPretty, pText, renderDoc)
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
import Octizys.Inference.BiDirectional
  ( solveExpressionTypeAddInfo
  )
import Octizys.Inference.Constraint
  ( Constraint
  , ConstraintId (ConstraintId')
  )
import Octizys.Inference.Context (Context, contextFromList, emptyContext)
import Octizys.Inference.Substitution (Substitution (Substitution))
import Octizys.Logging.Interpreters.Console (runLog)
import Octizys.Logging.Levels (Level (Info))
import qualified Octizys.Test.Inference.Substitution as C
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
  -> Ast.Expression Ast.TypeVariable
  -> [Constraint]
  -> Map TypeVariableId (Ast.MonoType Ast.TypeVariable)
  -> Type Ast.TypeVariable
  -> Type Ast.TypeVariable
  -> IO ()
assertEqualTypes ctx expr ast constraints subs t1 t2 =
  if typeEq t1 t2
    then pure ()
    else
      assertFailure $
        Text.unpack
          ( renderDoc $
              pretty ctx
                <> Pretty.hardline
                <> pretty constraints
                <> Pretty.hardline
                <> pText "Expected:"
                <> indentPretty t1
                <> Pretty.hardline
                <> pText "Result:"
                <> indentPretty t2
                <> Pretty.hardline
                <> pText "Original expression:"
                <> indentDoc (formatExpression defaultConfiguration expr)
                <> Pretty.hardline
                <> pText "Inferred expression:"
                <> indentDoc (pretty ast)
                <> Pretty.hardline
                <> pText "Substitution:"
                <> indentDoc (prettyMap subs)
          )
  where
    prettyMap m = Common.prettyItemList (Map.toList m) (pretty ',') (pretty '~')


runSolver
  :: Context
  -> Cst.Expression ExpressionVariableId TypeVariableId
  -> IO
      ( Either
          Text
          ( ( Ast.Expression Ast.TypeVariable
            , Map TypeVariableId (Ast.MonoType Ast.TypeVariable)
            )
          , [Constraint]
          )
      )
runSolver ctx expr =
  runEff $
    runConsole $
      runLog Info $
        runErrorNoCallStack $
          runReader ctx $
            runReader C.simbolContext $
              runAccumulatorFull [] $
                runIdGeneratoAlone 0 $
                  runStateAlone (ConstraintId' 0) (solveExpressionTypeAddInfo expr)
  where
    runStateAlone s ef = fst <$> runState s ef
    runIdGeneratoAlone s ef = fst <$> runIdGeneratorFull s ef


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
    , testCase "If constraints function argument to be bool" $ do
        counter <- newIORef 0
        nId <- C.makeExpressionVariableId counter
        let
          n = Cst.Variable C.sourceInfo nId
          expr =
            C.function
              (C.parameters (C.parameter nId Nothing) [])
              ( C.eIf (C.bool True) n (C.int 2)
              )
        assertInfers
          emptyContext
          expr
          ( Ast.TMono
              ( A.arrow [A.intType] A.intType
              )
          )
    , testCase "Multiplication example" $ do
        counter <- newIORef 0
        lt <- C.eVar counter
        mul <- C.eVar counter
        minus <- C.eVar counter
        nId <- C.makeExpressionVariableId counter
        let ltT = A.arrow [A.intType, A.intType] A.boolType
            mulT = A.arrow [A.intType, A.intType] A.intType
            minusT = A.arrow [A.intType, A.intType] A.intType
            n = Cst.Variable C.sourceInfo nId
            ctx =
              contextFromList
                [ (lt.name, Ast.TMono ltT)
                , (mul.name, Ast.TMono mulT)
                , (minus.name, Ast.TMono minusT)
                ]
                []
            expr =
              C.function
                (C.parameters (C.parameter nId Nothing) [])
                ( C.eIf
                    (C.app lt [n, C.int 1])
                    (C.int 1)
                    (C.app mul [n, C.int 2])
                )
        assertInfers ctx expr (Ast.TMono (A.arrow [A.intType] A.intType))
    , testCase "Identity" $ do
        counter <- newIORef 0
        nId <- C.makeExpressionVariableId counter
        tId <- C.makeTypeVariableId counter
        let
          n = Cst.Variable C.sourceInfo nId
          t = Variable (Ast.TypeVariable' tId)
          expr =
            C.function
              (C.parameters (C.parameter nId Nothing) [])
              n
        assertInfers emptyContext expr (A.scheme [tId] (A.arrow [t] t))
    , testCase "Partial application" $ do
        counter <- newIORef 0
        lt <- C.eVar counter
        nId <- C.makeExpressionVariableId counter
        let ltT = A.arrow [A.intType, A.intType] A.boolType
            n = Cst.Variable C.sourceInfo nId
            ctx =
              contextFromList
                [ (lt.name, Ast.TMono ltT)
                ]
                []
            expr =
              C.function
                (C.parameters (C.parameter nId Nothing) [])
                ( C.app lt [n]
                )
        assertInfers
          ctx
          expr
          (Ast.TMono (A.arrow [A.intType, A.intType] A.boolType))
    , testCase "Self application fails" $ do
        counter <- newIORef 0
        nId <- C.makeExpressionVariableId counter
        let
          n = Cst.Variable C.sourceInfo nId
          expr =
            C.function
              (C.parameters (C.parameter nId Nothing) [])
              ( C.app n [n]
              )
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
    Right ((result, subs), constraints) ->
      assertEqualTypes
        context
        expression
        result
        constraints
        subs
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
    Right ((result, _), _) ->
      assertFailure $
        Text.unpack $
          renderDoc
            ( pretty context
                <> Pretty.hardline
                <> Common.pText "Expected type error, but inferred: "
                <> pretty result
            )
    Left _ -> pure ()
