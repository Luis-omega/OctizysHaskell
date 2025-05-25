{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Octizys.Repl.Repl
  ( runRepl
  )
where

import Control.Arrow ((<<<))
import Effectful (Eff, runEff, (:>))
import Effectful.Error.Static
  ( Error
  , runErrorNoCallStack
  , runErrorNoCallStackWith
  )

-- import Octizys.Evaluation (EvaluationError)

import qualified Octizys.Effects.SymbolResolution.Effect as SRS
import qualified Octizys.Inference.ConstraintsGeneration as Inference
import qualified Octizys.Inference.ConstraintsSolver as Inference
import qualified Octizys.Inference.Errors as Inference

import Data.Functor (void)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import Data.Text (Text, pack)
import Effectful.State.Static.Local (State, get, gets, put, runState)
import Octizys.Ast.Evaluation (EvaluationError)
import qualified Octizys.Ast.Expression as Ast
import qualified Octizys.Cst.Expression as Cst
import qualified Octizys.Cst.Type as Cst
import Octizys.Effects.Console.Effect
  ( Console
  , putText
  , readLine
  )
import Octizys.Effects.Console.Interpreter
  ( putLine
  , runConsole
  )
import Octizys.Effects.Logger.ConsoleInterpreter (debug, runLog)
import Octizys.Effects.Logger.Effect (Logger)
import Octizys.Effects.Parser.Backend
  ( ParserError
  , prettyParserError
  )
import Octizys.Effects.Parser.Interpreter (runFullParser)
import Octizys.Effects.SymbolResolution.Effect
  ( SymbolResolution
  , putSymbolResolutionState
  )
import Octizys.Effects.SymbolResolution.Interpreter
  ( SymbolResolutionError
  , initialSymbolResolutionState
  , runSymbolResolution
  )
import Octizys.Parser.Common (OctizysParseError)
import Octizys.Repl.Ast
  ( ReplCommand (LoadFile, Quit)
  , ReplTop (Command, Define, Evaluate)
  )
import Octizys.Repl.Parser (replParser)
import Prettyprinter
  ( Doc
  , Pretty (pretty)
  , defaultLayoutOptions
  , layoutPretty
  )
import qualified Prettyprinter.Render.Text
import Text.Show.Pretty (ppShow)

import Cli
  ( ReplOptions
      ( formatterConfig
      , logLevel
      , showCst
      , showInference
      )
  )
import Control.Monad (forM_, when)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Octizys.Ast.Evaluation as Evaluation
import Octizys.Ast.Expression (buildDefinitionsMap)
import Octizys.Classes.FreeVariables (FreeTypeVariables (freeTyVars))
import Octizys.Classes.From (From (from))
import Octizys.Cst.Expression (ExpressionVariableId)
import Octizys.Cst.Node (Node)
import Octizys.Pretty.FormatContext
  ( FormatContext
  , formatExpressionVar
  , formatTypeVar
  , makeFormatContext
  , makeFormattersWithMap
  )
import Octizys.Pretty.Formatter (Formatter, format)
import Octizys.Report
  ( LongDescription (LongDescription', afterDescription, preDescription, source)
  , Report (Report', descriptions, reportKind, shortDescription)
  , ReportKind (ReportError)
  , prettyReport
  )
import qualified Prettyprinter as Pretty


data ReplStatus = Continue | Exit


newtype DefinedSymbols = DefinedSymbols'
  { definedSymbols :: Map ExpressionVariableId Ast.Expression
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Semigroup
    , Monoid
    )
    via (Map ExpressionVariableId Ast.Expression)


continue :: Eff es ReplStatus
continue = pure Continue


exit :: Eff es ReplStatus
exit = pure Exit


render :: forall a ann. (a -> Doc ann) -> a -> Text
render prettifier =
  Prettyprinter.Render.Text.renderStrict
    <<< Prettyprinter.layoutPretty Prettyprinter.defaultLayoutOptions
    <<< prettifier


buildReplFormatterContext
  :: State Inference.InferenceState :> es
  => ReplOptions
  -> Eff es (FormatContext ann)
buildReplFormatterContext opts = do
  istate <- gets Inference.expVarTable
  let
    fakeTypePrinter :: Int -> Doc ann
    formatters =
      makeFormattersWithMap
        (\x -> pretty @Text x.name)
        fakeTypePrinter
        istate
        mempty
    fakeTypePrinter = pretty
  pure $ makeFormatContext opts.formatterConfig formatters


formatE
  :: State Inference.InferenceState
    :> es
  => Formatter
      ann
      (FormatContext ann)
      a
  => ReplOptions
  -> a
  -> Eff es (Doc ann)
formatE opts value = do
  ctx <- buildReplFormatterContext opts
  pure $ format ctx value


rep
  :: ( Console :> es
     , SymbolResolution :> es
     , State Inference.InferenceState :> es
     , Error EvaluationError :> es
     , State DefinedSymbols :> es
     , Logger :> es
     )
  => ReplOptions
  -> Eff es ReplStatus
rep opts = do
  line <- putText "repl>" >> readLine
  maybeAction <- runFullParser line replParser
  -- s12 <- SRS.getSymbolResolutionState
  -- putLine $ pack $ ppShow s12.expNamesToId
  case maybeAction of
    Left e -> do
      putLine $
        render
          ( prettyParserError
              pretty
              (Just ('R' :| "epl"))
              line
          )
          e
      continue
    Right action ->
      case action of
        Command Quit ->
          putLine "Bye!"
            >> exit
        Command (LoadFile f) -> do
          putLine $ "Unsupported load of file: " <> f
          continue
        Evaluate expression ->
          do
            -- putLine $ render (Cst.prettyExpression pretty pretty) expression
            updateInferenceState
            when
              opts.showCst
              ( do
                  docExp <- formatE opts expression
                  putLine $ render id docExp
              )
            maybeAst <- runErrorNoCallStack (Inference.solveExpressionType expression)
            case maybeAst of
              Left e -> do
                ctx <- buildReplFormatterContext opts
                reportInferenceError
                  ctx
                  (from expression)
                  e
                  >> continue
              Right ast -> do
                updateSymbolState
                when
                  opts.showInference
                  ( do
                      docFinal <- formatE opts ast
                      putLine $ render id docFinal
                  )
                context <- updateDefinedSymbols ast
                result <- Evaluation.evaluateExpression context ast
                docResult <- formatE opts result
                putLine $ render id docResult
                continue
        Define d -> do
          when
            opts.showCst
            ( do
                docExp <- formatE opts d
                putLine $ render id docExp
            )
          updateInferenceState
          maybeAst <- runErrorNoCallStack $ Inference.solveDefinitionType d
          case maybeAst of
            Left e -> do
              ctx <- buildReplFormatterContext opts
              reportInferenceError ctx (from d) e >> continue
            Right ast -> do
              updateSymbolState
              when
                opts.showInference
                ( do
                    docAst <- formatE opts ast
                    putLine $ render id docAst
                )
              _ <- addDefinedSymbol ast
              continue
  where
    updateInferenceState
      :: SymbolResolution :> es
      => State Inference.InferenceState :> es
      => Eff es ()
    updateInferenceState = do
      currentSymbolState <- SRS.getSymbolResolutionState
      currentInference <- get
      put
        currentInference
          { Inference.expVarTable = SRS.expVarTable currentSymbolState
          , Inference.realVariablesMax =
              SRS.genVarType currentSymbolState + 1
          , Inference.constraintMap =
              currentInference.constraintMap
                { Inference.nextTypeVar = SRS.genVarType currentSymbolState + 1
                }
          }
    updateSymbolState
      :: SymbolResolution :> es
      => State Inference.InferenceState :> es
      => Eff es ()
    updateSymbolState = do
      currentInference <- get
      currentSymbolState <- SRS.getSymbolResolutionState
      putSymbolResolutionState
        currentSymbolState
          { SRS.expVarTable = Inference.expVarTable currentInference
          , SRS.genVarType =
              currentInference.constraintMap.nextTypeVar
                + 1
          }
    updateDefinedSymbols
      :: State DefinedSymbols :> es
      => Ast.Expression
      -> Eff es (Map ExpressionVariableId Ast.Expression)
    updateDefinedSymbols expr = do
      mp <- gets definedSymbols
      let exprSymbols = buildDefinitionsMap expr
          newMap = Map.union mp exprSymbols
      put $ DefinedSymbols' newMap
      pure newMap

    addDefinedSymbol
      :: State DefinedSymbols :> es
      => State Inference.InferenceState :> es
      => Logger :> es
      => Ast.Definition
      -> Eff es (Map ExpressionVariableId Ast.Expression)
    addDefinedSymbol d = do
      mp <- updateDefinedSymbols d.definition
      let addedSymbol = Map.insert d.name d.definition mp
      forM_
        (Map.toList addedSymbol)
        ( \(nam, ex) ->
            Inference.insertKnowType nam (Ast.getType ex)
        )
      let newMap = Map.insert d.name d.definition addedSymbol
      put $ DefinedSymbols' newMap
      pure newMap


report
  :: Console :> es
  => Report ann
  -> Eff es ()
report r =
  putLine $ render prettyReport r


buildInferenceErrorReport
  :: forall ann
   . FormatContext ann
  -> Node
  -> Inference.InferenceError
  -> Report ann
buildInferenceErrorReport ctx cst err =
  let
    (short, long) =
      case err of
        Inference.FunctionWithoutParams e ->
          let errorSource = format ctx e
           in ( "Found a function without parameters, this is a bug, please report it!"
              ,
                [ LongDescription'
                    { preDescription = Nothing
                    , source =
                        Just errorSource
                    , afterDescription = Just "All function must have at least one argument."
                    }
                ]
              )
        Inference.UnboundExpressionVar vid ->
          ( "Unknow variable found, this is a bug, please report it!"
          ,
            [ LongDescription'
                { preDescription =
                    Just
                      ( pack
                          ("Unknow variable: " <> show vid)
                      )
                , source =
                    Just $ format ctx cst
                , afterDescription =
                    Just
                      "At this point in the compilation, all the expression variables are know."
                }
            ]
          )
        Inference.CantUnify c -> do
          Inference.buildConstraintUnifyReportDescriptions
            ctx
            c
        Inference.ContainsFreeVariablesAfterSolving
          localCst
          ast
          freeVars
          subst ->
            let
              unsolveds = Inference.findUnsolvedSubstitutions freeVars subst
              related =
                filter
                  ( \s ->
                      Set.member s.variableId freeVars
                        || not
                          ( Set.null
                              (Set.intersection (freeTyVars s.value) freeVars)
                          )
                  )
                  unsolveds
              relevantConstraints =
                case related of
                  [] -> []
                  _ ->
                    [ LongDescription'
                        { preDescription = Just "The relevant constraints are:"
                        , source =
                            Just $
                              Pretty.list (format ctx <$> related)
                        , afterDescription = Nothing
                        }
                    ]
             in
              ( "InferenceError> Can't infer the type for expression."
              , [ LongDescription'
                    { preDescription = Just "The original code:"
                    , source =
                        Just $ format ctx localCst
                    , afterDescription = Nothing
                    }
                , LongDescription'
                    { preDescription = Just "Produces the annotated tree:"
                    , source =
                        Just $
                          format ctx ast
                    , afterDescription = Nothing
                    }
                , LongDescription'
                    { preDescription = Just "The unsolved variables are: "
                    , source = Just (formatSet freeVars)
                    , afterDescription = Nothing
                    }
                ]
                  <> relevantConstraints
              )
            where
              formatSet s =
                Pretty.list (formatTypeVar ctx <$> Set.toList s)
        Inference.RecursiveSubstitution sub ->
          ( "TypeError> Recursive types are forbidden."
          ,
            [ LongDescription'
                { preDescription =
                    Just
                      ( pack
                          ( "The variable _t"
                              <> show (Cst.unTypeVariableId sub.variableId)
                              <> " is recursive:"
                          )
                      )
                , source = Just $ format ctx sub
                , afterDescription = Nothing
                }
            , LongDescription'
                { preDescription =
                    Just
                      ( pack
                          "In the expression:"
                      )
                , source = Just $ format ctx sub.substitutionInfo.ast
                , afterDescription = Nothing
                }
            ]
          )
   in
    Report'
      { reportKind = ReportError
      , shortDescription = short
      , descriptions = long
      }


reportInferenceError
  :: SymbolResolution :> es
  => Console :> es
  => FormatContext ann
  -> Node
  -> Inference.InferenceError
  -> Eff es ()
reportInferenceError ctx cst err =
  let re = buildInferenceErrorReport ctx cst err
   in report re


reportErrorWith
  :: forall e es ann
   . Console :> es
  => (e -> Doc ann)
  -> Eff (Error e : es) ReplStatus
  -> Eff es ReplStatus
reportErrorWith prettier =
  runErrorNoCallStackWith
    (\e -> (putLine <<< render prettier) e >> continue)


reportErrorShow
  :: forall e es
   . (Console :> es, Show e)
  => Eff (Error e : es) ReplStatus
  -> Eff es ReplStatus
reportErrorShow = reportErrorWith (pretty <<< ppShow)


reportErrorParser
  :: forall es
   . Console :> es
  => Text
  -> Eff
      ( Error
          ( ParserError OctizysParseError
          )
          : es
      )
      ReplStatus
  -> Eff es ReplStatus
reportErrorParser source =
  reportErrorWith
    ( prettyParserError
        pretty
        (Just ('R' :| "epl"))
        source
    )


repl
  :: Console :> es
  => Logger :> es
  => ReplOptions
  -> Eff es ()
repl opts =
  ( void
      <<< runState @DefinedSymbols mempty
      <<< runState Inference.initialInferenceState
      <<< runState initialSymbolResolutionState
  )
    loop
  where
    loop = do
      status <-
        ( reportErrorShow @SymbolResolutionError
            <<< reportErrorShow @EvaluationError
            <<< runSymbolResolution
          )
          (rep opts)
      case status of
        Exit -> pure ()
        Continue -> loop


runRepl :: ReplOptions -> IO ()
runRepl options =
  ( runEff
      <<< runConsole
      <<< runLog options.logLevel
  )
    (repl options)
