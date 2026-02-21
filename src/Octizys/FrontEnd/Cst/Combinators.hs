{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module Octizys.FrontEnd.Cst.Combinators where

import Control.Arrow ((<<<))
import Data.IORef (IORef, readIORef, writeIORef)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromJust)
import Data.Text
import qualified Data.Text as Text
import EffectfulParserCombinators.Span (Span (Span'), makeInitialPosition)
import Octizys.Classes.From (From (from))
import Octizys.Common.Id
  ( ExpressionVariableId
  , GenerateFromInt (generateFromInt)
  , SymbolContext (SymbolContext')
  , TypeVariableId
  )
import Octizys.Common.LogicPath (LogicPath)
import qualified Octizys.Common.LogicPath as LogicPath
import Octizys.Common.Name (Name, makeName)
import Octizys.Common.Qualifier (Qualifier)
import Octizys.FrontEnd.Cst.Expression
  ( Annotation (Annotation')
  , Application (Application')
  , BoolExpression (BoolExpression')
  , Definition (Definition')
  , DefinitionTypeAnnotation (DefinitionTypeAnnotation')
  , Expression (EBool, EInt)
  , Function (Function')
  , If (If')
  , IntExpression (IntExpression')
  , Let (Let')
  , Parameter (ParameterAlone, ParameterWithType)
  , Parameters (..)
  , Parens (Parens')
  , SchemeStart (SchemeStart')
  )
import qualified Octizys.FrontEnd.Cst.Expression as Exp
import Octizys.FrontEnd.Cst.SourceInfo (SourceInfo, makeSourceInfo)
import Octizys.FrontEnd.Cst.Type
  ( Arrow (Arrow')
  , BoolType (BoolType')
  , IntType (IntType')
  , Type
    ( TBool
    , TInt
    , TVariable
    )
  )
import qualified Octizys.FrontEnd.Cst.Type as Type
import qualified Octizys.Package.Reference as Package


sourceInfo :: SourceInfo
sourceInfo =
  makeSourceInfo
    (Span' makeInitialPosition makeInitialPosition)
    []
    Nothing


pRef :: Package.Reference
pRef =
  Package.TargetPackage $
    Package.makeOptionalIdentity
      Nothing
      Nothing
      (Package.makeSource "test")


unsafeMakeName :: Text -> Name
unsafeMakeName = fromJust <<< makeName


simbolContext :: SymbolContext
simbolContext =
  let
    name = unsafeMakeName "Test"
    path :: LogicPath = LogicPath.singleton name
    qualifier :: Qualifier = from path
   in
    SymbolContext' pRef qualifier


boolType :: Type TypeVariableId
boolType = TBool $ BoolType' sourceInfo


intType :: Type TypeVariableId
intType = TInt $ IntType' sourceInfo


addSourceInfo :: Functor t => t a -> t (SourceInfo, a)
addSourceInfo x = (\y -> (sourceInfo, y)) <$> x


addSourceInfo2 :: Functor t => t a -> t (a, SourceInfo)
addSourceInfo2 x = (\y -> (y, sourceInfo)) <$> x


arrow :: [Type tvar] -> Type tvar -> Type tvar
arrow [] t = t
arrow (x : xs) out =
  from $
    Arrow'
      x
      ( NonEmpty.prependList
          (addSourceInfo xs)
          ((sourceInfo, out) NonEmpty.:| [])
      )


tParens :: Type tvar -> Type tvar
tParens x = from $ Type.Parens' sourceInfo x sourceInfo


makeVariable :: GenerateFromInt a => IORef Int -> IO a
makeVariable varCounter = do
  currentValue <- readIORef varCounter
  let
    tVari = generateFromInt simbolContext Nothing currentValue
  writeIORef varCounter (currentValue + 1)
  pure tVari


tVar :: IORef Int -> IO (Type TypeVariableId)
tVar counter = do
  newValue <- makeVariable counter
  pure $ TVariable (Type.Variable' sourceInfo newValue)


makeExpressionVariableId :: IORef Int -> IO ExpressionVariableId
makeExpressionVariableId varCounter = do
  currentValue <- readIORef varCounter
  let
    tVari = generateFromInt simbolContext Nothing currentValue
  writeIORef varCounter (currentValue + 1)
  pure tVari


parameter
  :: ExpressionVariableId
  -> Maybe (Type TypeVariableId)
  -> Parameter ExpressionVariableId TypeVariableId
parameter expr Nothing = ParameterAlone (sourceInfo, expr)
parameter expr (Just ty) = ParameterWithType (sourceInfo, expr) sourceInfo ty


parameters
  :: Parameter ExpressionVariableId TypeVariableId
  -> [Parameter ExpressionVariableId TypeVariableId]
  -> Parameters ExpressionVariableId TypeVariableId
parameters x y = Parameters' x (addSourceInfo y) sourceInfo


definitionAnnotation
  :: [TypeVariableId]
  -> Maybe (Parameters ExpressionVariableId TypeVariableId)
  -> Type TypeVariableId
  -> DefinitionTypeAnnotation ExpressionVariableId TypeVariableId
definitionAnnotation [] ps ty =
  DefinitionTypeAnnotation' sourceInfo Nothing ps ty
definitionAnnotation (x : xs) ps ty =
  DefinitionTypeAnnotation'
    sourceInfo
    ( Just $
        SchemeStart'
          sourceInfo
          (addSourceInfo (x NonEmpty.:| xs))
          sourceInfo
    )
    ps
    ty


definition
  :: ExpressionVariableId
  -> Maybe (DefinitionTypeAnnotation ExpressionVariableId TypeVariableId)
  -> Expression ExpressionVariableId TypeVariableId
  -> Definition ExpressionVariableId TypeVariableId
definition var mType =
  Definition'
    (sourceInfo, var)
    mType
    sourceInfo


bool :: Bool -> Expression ExpressionVariableId TypeVariableId
bool = EBool <<< BoolExpression' sourceInfo


int :: Int -> Expression ExpressionVariableId TypeVariableId
int = EInt <<< IntExpression' sourceInfo <<< Text.pack <<< show


eVar :: IORef Int -> IO (Exp.Variable ExpressionVariableId)
eVar counter = do
  newValue <- makeExpressionVariableId counter
  pure $ Exp.Variable' sourceInfo newValue


parens :: Expression tvar evar -> Expression tvar evar
parens x = from $ Parens' sourceInfo x sourceInfo


function
  :: Parameters tvar evar
  -> Expression tvar evar
  -> Expression tvar evar
function ps e = from $ Function' sourceInfo ps e


app
  :: Expression evar tvar
  -> [Expression evar tvar]
  -> Expression evar tvar
app start [] = start
app start (ini : remain) =
  from $
    Application'
      start
      (ini NonEmpty.:| remain)


eIf
  :: Expression evar tvar
  -> Expression evar tvar
  -> Expression evar tvar
  -> Expression evar tvar
eIf cond t = from <<< If' sourceInfo cond sourceInfo t sourceInfo


eLet
  :: [Definition evar tvar]
  -> Expression evar tvar
  -> Expression evar tvar
eLet [] body = body
eLet (start : remain) body =
  from $
    Let'
      sourceInfo
      ( addSourceInfo2
          (start NonEmpty.:| remain)
      )
      sourceInfo
      body


annotation
  :: Expression evar tvar
  -> Type tvar
  -> Expression evar tvar
annotation ex = from <<< Annotation' ex sourceInfo
