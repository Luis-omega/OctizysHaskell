{-# LANGUAGE DataKinds #-}

module Octizys.Test.Inference.BiDirectional where

import Data.IORef (IORef, newIORef)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import Effectful (runEff)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Reader.Static (runReader)
import Effectful.State.Static.Local (runState)
import GHC.Stack (HasCallStack)
import qualified Octizys.Ast.Combinators as A
import qualified Octizys.Ast.Expression as Ast
import Octizys.Ast.Type (Type)
import qualified Octizys.Ast.Type as Ast
import Octizys.Ast.Type.Basics
  ( InferenceVariable
  , TypeEq (typeEq)
  , TypeVariable (TypeVariable')
  )
import Octizys.Ast.Type.MonoType (MonoType (MonoVariable))
import Octizys.Classes.From (from)
import Octizys.Common.Id
  ( ExpressionVariableId
  , TypeVariableId
  )
import Octizys.Effects.Accumulator.Interpreter (runAccumulatorFull)
import Octizys.Effects.Console.Interpreter (runConsole)
import Octizys.Effects.IdGenerator.Interpreter (runIdGeneratorFull)
import Octizys.Format.Class (Formattable (format))
import Octizys.Format.Config (defaultConfiguration)
import Octizys.Format.Utils
  ( formatListWith
  , formatMapWith
  , formatWithHeader
  , indentDoc
  , indentFormat
  , renderDoc
  , text
  )
import qualified Octizys.FrontEnd.Cst.Combinators as C
import qualified Octizys.FrontEnd.Cst.Expression as Cst
import qualified Octizys.FrontEnd.Cst.Node as Cst
import Octizys.Inference.BiDirectional
  ( solveExpressionTypeAddInfo
  )
import Octizys.Inference.Constraint
  ( Constraint
  , ConstraintId (ConstraintId')
  )
import Octizys.Inference.Context (Context, contextFromList, emptyContext)
import Octizys.Logging.Interpreters.Console (runLog)
import Octizys.Logging.Levels (Level (Info))
import qualified Octizys.Test.Inference.Substitution as C
import Prettyprinter (Pretty (pretty), (<+>))
import qualified Prettyprinter as Pretty
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion
  , assertFailure
  , testCase
  )


makeMonoVar :: IORef Int -> IO (MonoType InferenceVariable)
makeMonoVar counter =
  MonoVariable <$> C.makeVariable counter


assertEqualTypes
  :: Context
  -> Cst.Expression ExpressionVariableId TypeVariableId
  -> Ast.Expression TypeVariable
  -> [Constraint]
  -> Map TypeVariableId (MonoType TypeVariable)
  -> Type TypeVariable
  -> Type TypeVariable
  -> IO ()
assertEqualTypes ctx expr ast constraints subs t1 t2 =
  if typeEq t1 t2
    then pure ()
    else
      assertFailure $
        Text.unpack
          ( renderDoc $
              format defaultConfiguration ctx
                <> Pretty.hardline
                <> formatListWith defaultConfiguration format constraints
                <> Pretty.hardline
                <> text "Expected:"
                <> indentFormat defaultConfiguration t1
                <> Pretty.hardline
                <> text "Result:"
                <> indentFormat defaultConfiguration t2
                <> Pretty.hardline
                <> text "Original expression:"
                <> indentDoc defaultConfiguration (format defaultConfiguration expr)
                <> Pretty.hardline
                <> text "Inferred expression:"
                <> indentDoc defaultConfiguration (format defaultConfiguration ast)
                <> Pretty.hardline
                <> text "Substitution:"
                <> indentDoc defaultConfiguration (formatSub subs)
          )
  where
    formatSub =
      formatMapWith
        defaultConfiguration
        (\c (x, y) -> format c x <+> pretty '~' <+> format c y)
        (pretty ',')


runSolver
  :: Context
  -> Cst.Expression ExpressionVariableId TypeVariableId
  -> IO
      ( Either
          Text
          ( ( Ast.Expression TypeVariable
            , Map TypeVariableId (MonoType TypeVariable)
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
          t = MonoVariable (TypeVariable' tId)
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
  -> Ast.Type TypeVariable
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
            ( format defaultConfiguration context
                <> Pretty.hardline
                <> formatWithHeader
                  defaultConfiguration
                  "Can't infer a type, but a type was expected!"
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
            ( format defaultConfiguration context
                <> Pretty.hardline
                <> text "Expected type error, but inferred: "
                <> format defaultConfiguration result
            )
    Left _ -> pure ()
