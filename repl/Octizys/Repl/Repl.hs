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
import qualified Octizys.Inference.Inference as Inference

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
import qualified Octizys.Pretty.Expression as Cst
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

import Cli (ReplOptions (logLevel, showCst, showInference))
import Control.Monad (forM_, when)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Octizys.Ast.Evaluation as Evaluation
import Octizys.Ast.Expression (buildDefinitionsMap)
import Octizys.Cst.Expression (ExpressionVariableId)
import Octizys.Cst.InfoId (InfoSpan)
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


prettyExpression
  :: State Inference.InferenceState :> es
  => Ast.Expression
  -> Eff es (Doc ann)
prettyExpression e = do
  ist <- gets Inference.expVarTable
  pure $
    Ast.prettyExpressionWithDic
      ist
      (\x -> pretty @Text x.name)
      e


prettyCstExpression
  :: SymbolResolution :> es
  => Cst.Expression
  -> Eff es (Doc ann)
prettyCstExpression e = do
  srs <- SRS.getSymbolResolutionState
  pure $
    Cst.prettyExpressionWithDic
      srs.expVarTable
      (\x -> pretty @Text x.name)
      e


prettyCstDefinition
  :: SymbolResolution :> es
  => Cst.Definition
  -> Eff es (Doc ann)
prettyCstDefinition ast = do
  srs <- SRS.getSymbolResolutionState
  pure $
    Cst.prettyDefinitionWithDic
      srs.expVarTable
      (\x -> pretty @Text x.name)
      ast


prettyDefinition
  :: SymbolResolution :> es
  => Ast.Definition
  -> Eff es (Doc ann)
prettyDefinition ast = do
  srs <- SRS.getSymbolResolutionState
  pure $
    Ast.prettyDefinitionWithDic
      srs.expVarTable
      (\x -> pretty @Text x.name)
      ast


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
                  docExp <- prettyCstExpression expression
                  putLine $ render id docExp
              )
            maybeAst <- runErrorNoCallStack (Inference.solveExpressionType expression)
            case maybeAst of
              Left e ->
                reportInferenceError
                  line
                  (Left expression)
                  e
                  >> continue
              Right ast -> do
                updateSymbolState
                when
                  opts.showInference
                  ( do
                      docFinal <- prettyExpression ast
                      putLine $ render id docFinal
                  )
                context <- updateDefinedSymbols ast
                result <- Evaluation.evaluateExpression context ast
                docResult <- prettyExpression result
                putLine $ render id docResult
                continue
        Define d -> do
          when
            opts.showCst
            ( do
                docExp <- prettyCstDefinition d
                putLine $ render id docExp
            )
          updateInferenceState
          maybeAst <- runErrorNoCallStack $ Inference.solveDefinitionType d
          case maybeAst of
            Left e -> reportInferenceError line (Right d) e >> continue
            Right ast -> do
              updateSymbolState
              when
                opts.showInference
                ( do
                    docAst <- prettyDefinition ast
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
      debug $
        pretty @Text "adding: " <> Pretty.list (pretty <$> Map.toList addedSymbol)
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


prettyLocate
  :: InfoSpan
  -> Cst.Expression
  -> Cst.Expression
prettyLocate info cst =
  case Cst.locateInfoSpan info cst of
    Just errorExpression -> errorExpression
    _ -> cst


buildInferenceErrorReport
  :: forall es ann
   . SymbolResolution :> es
  => Text
  -> Either Cst.Expression Cst.Definition
  -> Inference.InferenceError
  -> Eff es (Report ann)
buildInferenceErrorReport src cst err = do
  (short, long) <-
    case err of
      Inference.FunctionWithoutParams e -> do
        errorSource <- prettyCstExpression e
        pure
          ( "Found a function without parameters, this is a bug, please report it!"
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
        pure
          ( "Unknow variable found, this is a bug, please report it!"
          ,
            [ LongDescription'
                { preDescription =
                    Just
                      ( pack
                          ("Unknow variable: " <> show vid)
                      )
                , source =
                    Just $
                      either
                        (Cst.prettyExpression pretty pretty)
                        (Cst.prettyDefinition pretty pretty)
                        cst
                , afterDescription =
                    Just
                      "At this point in the compilation, all the expression variables are know."
                }
            ]
          )
      Inference.CantUnify c -> do
        srs <- SRS.getSymbolResolutionState
        let prettyVar var =
              case Map.lookup var srs.expVarTable of
                Just x -> pretty @Text x.name
                Nothing -> pretty var
            locationCstDoc =
              case cst of
                Left cstE -> Cst.prettyExpression prettyVar pretty $
                  case Cst.locateInfoSpan c.info cstE of
                    Just errorCst -> errorCst
                    _ -> cstE
                Right def -> Cst.prettyDefinition prettyVar pretty def
        pure $
          Inference.buildConstraintUnifyReportDescriptions
            locationCstDoc
            c
      Inference.ExpressionContainsFreeVariablesAfterSolving
        ast
        localCst
        freeVars
        subst ->
          pure
            ( "InferenceError> Can't infer the type for expression."
            ,
              [ LongDescription'
                  { preDescription = Just "The original code:"
                  , source =
                      Just $
                        Cst.prettyExpression pretty pretty localCst
                  , afterDescription = Nothing
                  }
              , LongDescription'
                  { preDescription = Just "Produces the annotated tree:"
                  , source =
                      Just $
                        pretty ast
                  , afterDescription = Nothing
                  }
              , LongDescription'
                  { preDescription = Just "The unsolved variables are: "
                  , source = Just (prettySet freeVars)
                  , afterDescription = Nothing
                  }
              , LongDescription'
                  { preDescription = Just "The relevant constraints are:"
                  , source =
                      Just
                        ( let
                            unsolveds = Inference.findUnsolvedSubstitutions freeVars subst
                           in
                            Pretty.list (pretty <$> unsolveds)
                        )
                  , afterDescription = Nothing
                  }
              ]
            )
          where
            prettySet s =
              Pretty.list (pretty <$> Set.toList s)
      Inference.DefinitionContainsFreeVariablesAfterSolving
        ast
        cstDef
        freeVars
        subst ->
          pure
            ( "InferenceError> Can't infer the type for expression."
            ,
              [ LongDescription'
                  { preDescription = Just "The original code:"
                  , source =
                      Just $
                        Cst.prettyDefinition pretty pretty cstDef
                  , afterDescription = Nothing
                  }
              , LongDescription'
                  { preDescription = Just "Produces the annotated tree:"
                  , source =
                      Just $
                        pretty ast
                  , afterDescription = Nothing
                  }
              , LongDescription'
                  { preDescription = Just "The unsolved variables are: "
                  , source = Just (prettySet freeVars)
                  , afterDescription = Nothing
                  }
              , LongDescription'
                  { preDescription = Just "The relevant constraints are:"
                  , source =
                      Just
                        ( let
                            unsolveds = Inference.findUnsolvedSubstitutions freeVars subst
                           in
                            Pretty.list (pretty <$> unsolveds)
                        )
                  , afterDescription = Nothing
                  }
              ]
            )
          where
            prettySet s =
              Pretty.list (pretty <$> Set.toList s)
      Inference.RecursiveSubstitution _ info vid newType -> do
        srs <- SRS.getSymbolResolutionState
        let prettyVar var =
              case Map.lookup var srs.expVarTable of
                Just x -> pretty @Text x.name
                Nothing -> pretty var
        pure
          ( "TypeError> Recursive types are forbidden."
          ,
            [ LongDescription'
                { preDescription =
                    Just
                      ( pack
                          ("The variable _t" <> show (Cst.unTypeVariableId vid) <> " is recursive:")
                      )
                , source = Just (pretty vid <> pretty '~' <> pretty newType)
                , afterDescription = Nothing
                }
            , LongDescription'
                { preDescription =
                    Just
                      ( pack
                          "In the expression:"
                      )
                , source =
                    Just $
                      either
                        (Cst.prettyExpression prettyVar pretty <<< prettyLocate info)
                        (Cst.prettyDefinition prettyVar pretty)
                        cst
                , afterDescription = Nothing
                }
            ]
          )
  pure $
    Report'
      { reportKind = ReportError
      , shortDescription = short
      , descriptions = long
      }


reportInferenceError
  :: SymbolResolution :> es
  => Console :> es
  => Text
  -> Either Cst.Expression Cst.Definition
  -> Inference.InferenceError
  -> Eff es ()
reportInferenceError src cst err =
  buildInferenceErrorReport src cst err
    >>= report


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


reportErrorPretty
  :: forall e es
   . (Console :> es, Pretty e)
  => Eff (Error e : es) ReplStatus
  -> Eff es ReplStatus
reportErrorPretty = reportErrorWith pretty


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
            <<< reportErrorPretty @EvaluationError
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
