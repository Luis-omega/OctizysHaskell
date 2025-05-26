{-# LANGUAGE AllowAmbiguousTypes #-}
module Octizys.Compiler.Format where

import Control.Arrow ((<<<))
import Effectful (Eff, (:>))

-- import Octizys.Evaluation (EvaluationError)

import qualified Octizys.Effects.SymbolResolution.Effect as SRS
import qualified Octizys.Inference.ConstraintsGeneration as Inference
import qualified Octizys.Inference.ConstraintsSolver as Inference
import qualified Octizys.Inference.Errors as Inference

import Data.Text (Text, pack)
import Effectful.State.Static.Local (State, gets)
import qualified Octizys.Cst.Type as Cst
import Octizys.Effects.Console.Effect
  ( Console
  )
import Octizys.Effects.Console.Interpreter
  ( putLine
  )
import Prettyprinter
  ( Doc
  , Pretty (pretty)
  , defaultLayoutOptions
  , layoutPretty
  )
import qualified Prettyprinter.Render.Text

import qualified Data.Set as Set
import Octizys.Classes.FreeVariables (FreeTypeVariables (freeTyVars))
import Octizys.Cst.Node (Node)
import Octizys.Pretty.FormatContext
  ( Configuration
  , FormatContext
  , formatTypeVar
  , makeFormatContext
  , makeFormattersWithMap
  )
import Octizys.Pretty.Formatter (Formatter, format)
import Octizys.Report
  ( LongDescription (LongDescription', afterDescription, preDescription, source)
  , Report (Report', descriptions, reportKind, shortDescription)
  , ReportKind (ReportError)
  )
import qualified Prettyprinter as Pretty
import Octizys.Effects.SymbolResolution.Effect (SymbolResolution)


buildFormatContext
  :: State Inference.InferenceState :> es
  => Configuration
  -> Eff es (FormatContext ann)
buildFormatContext config = do
  istate <- gets Inference.expVarTable
  let
    fakeTypePrinter :: Int -> Doc ann
    fakeTypePrinter = pretty

    formatters =
      makeFormattersWithMap
        (\x -> pretty @Text x.name)
        fakeTypePrinter
        istate
        mempty
  pure $ makeFormatContext config formatters

buildFormatContextFromSymbolResolution
  :: SymbolResolution :> es
  => Configuration
  -> Eff es (FormatContext ann)
buildFormatContextFromSymbolResolution config = do
  srst <- SRS.getSymbolResolutionState
  let
    mp = srst.expVarTable
    fakeTypePrinter :: Int -> Doc ann
    fakeTypePrinter = pretty

    formatters =
      makeFormattersWithMap
        (\x -> pretty @Text x.name)
        fakeTypePrinter
        mp
        mempty
  pure $ makeFormatContext config formatters


formatE
  :: State Inference.InferenceState
    :> es
  => Formatter
      ann
      (FormatContext ann)
      a
  => Configuration
  -> a
  -> Eff es (Doc ann)
formatE config value = do
  ctx <- buildFormatContext config
  pure $ format ctx value


render :: forall ann. Doc ann -> Text
render =
  Prettyprinter.Render.Text.renderStrict
    <<< Prettyprinter.layoutPretty Prettyprinter.defaultLayoutOptions

pprint
  :: forall a ann es
   . Console :> es
  => State Inference.InferenceState :> es
  => Formatter ann (FormatContext ann) a
  => Configuration
  -> a
  -> Eff es ()
pprint config value = do
  doc <- formatE @es @ann @a config value
  putLine $ render doc


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

