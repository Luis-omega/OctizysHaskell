{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Octizys.Inference.Translation
  ( ExpressionVariable (ExpressionVariableC)
  , TypeVariable (UserDeclaredVar, MetaVar, BoundToExpression)
  , InferenceExpression
  , InferenceType
  , TranslationState
  , TranslationError
  , TranslationWarning
  , buildInferenceContext
  , transformType
  , transform
  , VariableInformation
  , informationTypeVariableId
  , emptyTranslationState
  , TypeVarBoundToExprVar
  , TypeVarMeta
  , TypeVarDeclaredByUser
  )
where

import Control.Arrow ((<<<))
import Control.Monad (unless)
import Data.Map (Map)
import qualified Data.Map as Map
import Effectful (Eff, (:>))
import Effectful.Error.Dynamic (Error, throwError)
import Effectful.State.Static.Local (State, get, gets, modify, put)
import Effectful.Writer.Static.Local (Writer, tell)
import Octizys.Ast
  ( Expression
      ( Annotation
      , Application
      , Function
      , If
      , Let
      , LiteralExpression
      , Variable
      , annotationExpression
      , annotationType
      , applicationArguments
      , applicationFunction
      , functionBody
      , functionParameters
      , ifCondition
      , ifElse
      , ifThen
      , letDefinitions
      , letIn
      )
  , LetDefinition (LetDefinitionC, letDefinition, letName)
  , TopItem (topItemBody, topItemName)
  , Type (Arrow, BoolType, IntType, TypeVar, arrowEnd, arrowInitial)
  , makeIf
  )
import Octizys.Effects.Generator (GenerateFromInt, Generator, generate)
import Octizys.HistoryMap (HistoryMap)
import qualified Octizys.HistoryMap as HistoryMap
import qualified Octizys.Parser as Parser


{- | A variable that is bound to a expression variable in the source
 code, those are generated automatically.
-}
newtype TypeVarBoundToExprVar = TypeVarBoundToExprVarC {wrapper :: Int}
  deriving (Show, Eq, Ord)
  deriving (GenerateFromInt) via Int


-- | A type variable that a user introduced explicitly.
newtype TypeVarDeclaredByUser = TypeVarDeclaredByUserC {wrapper2 :: Int}
  deriving (Show, Eq, Ord)
  deriving (GenerateFromInt) via Int


-- | Metavariables used in the inference process
newtype TypeVarMeta = TypeVarMetaC {wrapper3 :: Int}
  deriving (Show, Eq, Ord)
  deriving (GenerateFromInt) via Int


data TypeVariable
  = -- | assigned to some expression variable
    BoundToExpression TypeVarBoundToExprVar
  | -- | variable introduced artificially
    MetaVar TypeVarMeta
  | -- | introduced explicitly by the user
    UserDeclaredVar TypeVarDeclaredByUser
  deriving (Show, Eq, Ord)


newtype ExpressionVariable = ExpressionVariableC Int
  deriving (Show, Eq, Ord)
  deriving (GenerateFromInt) via Int


type InferenceExpression =
  Expression ExpressionVariable TypeVariable


type InferenceType = Type TypeVariable


{- | Any error that can happen in the translation for the parsed ast
to a inference ast.
-}
data TranslationError
  = UnboundedVariable Parser.ExpressionVariable
  | MultipleDefinitions [Parser.ExpressionVariable]
  deriving (Show)


-- TODO : raise Shadowing concerns
data TranslationWarning = Warn deriving (Show)


{- | Stores all the info related to a expression variable in the
source file.
-}
data VariableInformation = VariableInformationC
  { informationName :: Parser.ExpressionVariable
  , informationExpressionVariableId :: ExpressionVariable
  , informationTypeVariableId :: TypeVariable
  , -- Parameters Doesn't have a expression dedicated to them
    informationExpression :: Maybe InferenceExpression
  }
  deriving (Show)


data TranslationState = TranslationStateC
  { nameToInfoMap :: HistoryMap Parser.ExpressionVariable VariableInformation
  , idToInfoMap :: Map ExpressionVariable VariableInformation
  }
  deriving (Show)


emptyTranslationState :: TranslationState
emptyTranslationState =
  TranslationStateC
    { nameToInfoMap = HistoryMap.empty
    , idToInfoMap = Map.empty
    }


{- | Checks that the list of variables doesn't
have repetitions, if it has, we throw a error.
-}
checkUniqueVariables
  :: ( Error TranslationError :> es
     , State TranslationState :> es
     )
  => [Parser.ExpressionVariable]
  -> Eff es ()
checkUniqueVariables pevs =
  let frequencies =
        Map.toList
          ( Map.fromListWith
              (+)
              ( (\x -> (x, 1 :: Int)) <$> pevs
              )
          )
      repeateds = filter (\x -> snd x > 1) frequencies
   in unless
        (null repeateds)
        $ throwError
          (MultipleDefinitions (fst <$> repeateds))


checkNoDefinedVariable
  :: ( Error TranslationError :> es
     , State TranslationState :> es
     )
  => Parser.ExpressionVariable
  -> Eff es ()
checkNoDefinedVariable var = do
  nameContext <- gets nameToInfoMap
  case HistoryMap.lookup var nameContext of
    Just _ -> throwError (MultipleDefinitions [var])
    Nothing -> pure ()


checkNoDefinedVariables
  :: ( Error TranslationError :> es
     , State TranslationState :> es
     )
  => [Parser.ExpressionVariable]
  -> Eff es ()
checkNoDefinedVariables = mapM_ checkNoDefinedVariable


freshVariableInformation
  :: ( State TranslationState :> es
     , Generator TypeVarBoundToExprVar :> es
     , Generator ExpressionVariable :> es
     )
  => Parser.ExpressionVariable
  -> Eff es VariableInformation
freshVariableInformation pev = do
  newEVar <- generate
  newTVar <- BoundToExpression <$> generate
  pure $
    VariableInformationC
      { informationName = pev
      , informationExpressionVariableId = newEVar
      , informationTypeVariableId = newTVar
      , informationExpression = Nothing
      }


{- | Registers a all the variables in
| the context with a row that has empty
| the expression field
-}
registerEmpty
  :: ( Error TranslationError :> es
     , State TranslationState :> es
     , Generator TypeVarBoundToExprVar :> es
     , Generator ExpressionVariable :> es
     )
  => [Parser.ExpressionVariable]
  -> Eff es [ExpressionVariable]
registerEmpty pevs = do
  checkUniqueVariables pevs
  checkNoDefinedVariables pevs
  rows <-
    mapM
      ( \item ->
          freshVariableInformation item >>= \x -> pure (item, x)
      )
      pevs
  translationState <- get
  let newNameContext = HistoryMap.pushChanges rows (nameToInfoMap translationState)
  let newIdContext =
        foldr
          ( \(_, row) dic ->
              Map.insert
                (informationExpressionVariableId row)
                row
                dic
          )
          (idToInfoMap translationState)
          rows
  put
    TranslationStateC
      { nameToInfoMap =
          newNameContext
      , idToInfoMap = newIdContext
      }
  pure ((informationExpressionVariableId <<< snd) <$> rows)


{- | Get the information of a Expression variable
if not found it raises a error
-}
lookupVariable
  :: ( Error TranslationError :> es
     , State TranslationState :> es
     )
  => Parser.ExpressionVariable
  -> Eff es VariableInformation
lookupVariable parserVar = do
  nameContext <- gets nameToInfoMap
  case HistoryMap.lookup parserVar nameContext of
    Just row -> pure row
    Nothing -> throwError (UnboundedVariable parserVar)


cleanupParameters
  :: ( Error TranslationError :> es
     , State TranslationState :> es
     )
  => Eff es ()
cleanupParameters = modify $ \s ->
  let
    byName = nameToInfoMap s
    newByName = HistoryMap.popChanges byName
   in
    s {nameToInfoMap = newByName}


cleanupLetNames
  :: ( Error TranslationError :> es
     , State TranslationState :> es
     )
  => [(Parser.ExpressionVariable, InferenceExpression)]
  -> Eff es ()
cleanupLetNames defs = do
  mapM_ (uncurry addExpressionToVariableInformation) defs
  cleanupParameters


transformType
  :: ( Error TranslationError :> es
     , Writer [TranslationWarning] :> es
     , State TranslationState :> es
     )
  => Parser.Type
  -> Eff es InferenceType
transformType t =
  case t of
    IntType -> pure IntType
    BoolType -> pure BoolType
    Arrow {arrowInitial, arrowEnd} -> do
      ini <- transformType arrowInitial
      end <- mapM transformType arrowEnd
      pure Arrow {arrowInitial = ini, arrowEnd = end}
    -- TODO: raise error here? we don't support type vars
    -- in the parser or anywhere else yet.
    TypeVar _ -> (pure <<< TypeVar <<< MetaVar <<< TypeVarMetaC) (-1)


transformLetDefinition
  :: ( Error TranslationError :> es
     , Writer [TranslationWarning] :> es
     , State TranslationState :> es
     , Generator TypeVarBoundToExprVar :> es
     , Generator ExpressionVariable :> es
     )
  => LetDefinition Parser.ExpressionVariable Parser.TypeVariable
  -> Eff es (LetDefinition ExpressionVariable TypeVariable)
transformLetDefinition
  LetDefinitionC
    { letName =
      _letName
    , letDefinition = _letDefinition
    } = do
    -- We assume the main let transform already registered
    -- the name!
    newName <- informationExpressionVariableId <$> lookupVariable _letName
    newDefinition <- transform _letDefinition
    pure $
      LetDefinitionC
        { letName = newName
        , letDefinition = newDefinition
        }


transform
  :: ( Error TranslationError :> es
     , Writer [TranslationWarning] :> es
     , State TranslationState :> es
     , Generator TypeVarBoundToExprVar :> es
     , Generator ExpressionVariable :> es
     )
  => Parser.Expression
  -> Eff es InferenceExpression
transform expr =
  case expr of
    LiteralExpression l -> pure $ LiteralExpression l
    Variable v -> do
      value <- lookupVariable v
      pure $ Variable (informationExpressionVariableId value)
    Function {functionParameters = _parameters, functionBody = _body} -> do
      newParameters <- registerEmpty _parameters
      newBody <- transform _body
      cleanupParameters
      pure
        Function {functionParameters = newParameters, functionBody = newBody}
    Application
      { applicationFunction = appFunction
      , applicationArguments =
        appArguments
      } -> do
        newFunction <- transform appFunction
        newArguments <- mapM transform appArguments
        pure
          Application
            { applicationFunction = newFunction
            , applicationArguments = newArguments
            }
    If {ifCondition = condition, ifThen = _then, ifElse = _else} -> do
      newIfCondition <- transform condition
      newIfThen <- transform _then
      newIfElse <- transform _else
      pure $ makeIf newIfCondition newIfThen newIfElse
    Let {letDefinitions = definitions, letIn = _letIn} -> do
      let names = letName <$> definitions
      _ <- registerEmpty names
      -- TODO : we should accumulate the errors on every
      -- definition, then run anyways the transformation of
      -- the In part and finally report all them at this point.
      newDefinitions <- mapM transformLetDefinition definitions
      newIn <- transform _letIn
      cleanupLetNames (zip names (letDefinition <$> newDefinitions))
      pure
        Let
          { letDefinitions = newDefinitions
          , letIn = newIn
          }
    Annotation {annotationExpression = ae, annotationType = at} -> do
      newExpression <- transform ae
      newType <- transformType at
      pure
        Annotation
          { annotationExpression =
              newExpression
          , annotationType = newType
          }


{- | It updates the entry of a variable in
| the context. Raises an error if
| unbound
| It doesn't update the scope context (nameToInfoMap).
-}
addExpressionToVariableInformation
  :: ( Error TranslationError :> es
     , State TranslationState :> es
     )
  => Parser.ExpressionVariable
  -> InferenceExpression
  -> Eff es ()
addExpressionToVariableInformation pev ie = do
  row <- lookupVariable pev
  infoState <- get
  let updatedVariableInformation = row {informationExpression = Just ie}
  let updatedMap =
        Map.insert
          (informationExpressionVariableId row)
          updatedVariableInformation
          (idToInfoMap infoState)
  put
    infoState
      { idToInfoMap = updatedMap
      }


buildInferenceContextForItem
  :: ( Error TranslationError :> es
     , Writer [TranslationWarning] :> es
     , State TranslationState :> es
     , Generator TypeVarBoundToExprVar :> es
     , Generator ExpressionVariable :> es
     )
  => Parser.TopItem
  -> Eff es ()
buildInferenceContextForItem item = do
  let name = Parser.ExpressionVariableC $ topItemName item
  let body = topItemBody item
  newBody <- transform body
  addExpressionToVariableInformation name newBody


{- | Use it to add a lot of definitions at the
beginning of context. Don't use later!
-}
buildInferenceContext
  :: ( Error TranslationError :> es
     , Writer
        [TranslationWarning]
        :> es
     , State TranslationState :> es
     , Generator TypeVarBoundToExprVar :> es
     , Generator ExpressionVariable :> es
     )
  => [Parser.TopItem]
  -> Eff es (Map ExpressionVariable VariableInformation)
buildInferenceContext items = do
  tell [Warn]
  _ <-
    registerEmpty $
      (Parser.ExpressionVariableC <<< topItemName) <$> items
  mapM_ buildInferenceContextForItem items
  idToInfoMap <$> get
