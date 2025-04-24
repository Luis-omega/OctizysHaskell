{-# LANGUAGE FlexibleContexts #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Octizys.Inference.Translation
  ( InferenceExpressionVar (InferenceExpressionVarC)
  , InferenceTypeVar (UserDeclaredVar, MetaVar, RealExpressionVar)
  , InferenceExpression
  , InferenceType
  , TranslationState
  , TranslationError
  , TranslationWarning
  , buildInferenceContext
  , transformType
  , transform
  , emptyState
  , Row
  , rowTypeVariable
  , Context
  )
where

import Control.Arrow ((<<<))
import Control.Monad (unless)
import Data.Map (Map)
import qualified Data.Map as Map
import Effectful (Eff, (:>))
import Effectful.Error.Dynamic (Error, throwError)
import Effectful.State.Static.Local (State, get, gets, modify)
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
  , ParserExpression
  , ParserExpressionVariable (ParserNamedVariable)
  , ParserTopItem
  , ParserType
  , ParserTypeVariable
  , Symbol
  , TopItem (topItemBody, topItemName)
  , Type (Arrow, BoolType, IntType, TypeVar, arrowEnd, arrowInitial)
  , makeIf
  )
import Octizys.HistoryMap (HistoryMap)
import qualified Octizys.HistoryMap as HistoryMap


data InferenceTypeVar
  = -- | assigned to some expression variable
    RealExpressionVar Int
  | -- | variable introduced artificially
    MetaVar Int
  | -- | introduced explicitly by the user
    UserDeclaredVar Int Symbol
  deriving (Show, Eq, Ord)


newtype InferenceExpressionVar = InferenceExpressionVarC Int
  deriving (Show, Eq, Ord)


type InferenceExpression =
  Expression InferenceExpressionVar InferenceTypeVar


type InferenceType = Type InferenceTypeVar


data TranslationError
  = UnboundedVariable ParserExpressionVariable
  | MultipleDefinitions [ParserExpressionVariable]
  deriving (Show)


-- TODO : raise Shadowing concerns
data TranslationWarning = Warn deriving (Show)


data Row = RowC
  { rowParserVariable :: ParserExpressionVariable
  , rowExpressionVariable :: InferenceExpressionVar
  , rowTypeVariable :: InferenceTypeVar
  , -- Parameters Doesn't have a expression dedicated to them
    rowExpression :: Maybe InferenceExpression
  }
  deriving (Show)


data Context = ContextC
  { contextByName :: HistoryMap ParserExpressionVariable Row
  , contextById :: Map InferenceExpressionVar Row
  }
  deriving (Show)


emptyContext :: Context
emptyContext =
  ContextC
    { contextByName = HistoryMap.empty
    , contextById = Map.empty
    }


data TranslationState = TranslationState
  { translationContext :: Context
  , nextExpressionVarId :: Int
  , nextTypeVarId :: Int
  }
  deriving (Show)


emptyState :: TranslationState
emptyState =
  TranslationState
    { translationContext =
        emptyContext
    , nextExpressionVarId = 0
    , nextTypeVarId = 0
    }


{- | Checks that the list of variables doesn't
have repetitions, if it has, we throw a error.
-}
checkUniqueVariables
  :: ( Error TranslationError :> es
     , State TranslationState :> es
     )
  => [ParserExpressionVariable]
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
  => ParserExpressionVariable
  -> Eff es ()
checkNoDefinedVariable var = do
  nameContext <- gets (contextByName <<< translationContext)
  case HistoryMap.lookup var nameContext of
    Just _ -> throwError (MultipleDefinitions [var])
    Nothing -> pure ()


checkNoDefinedVariables
  :: ( Error TranslationError :> es
     , State TranslationState :> es
     )
  => [ParserExpressionVariable]
  -> Eff es ()
checkNoDefinedVariables = mapM_ checkNoDefinedVariable


{- | Registers a all the variables in
| the context with a row that has empty
| the expression field
-}
registerEmpty
  :: ( Error TranslationError :> es
     , State TranslationState :> es
     )
  => [ParserExpressionVariable]
  -> Eff es [InferenceExpressionVar]
registerEmpty pevs = do
  checkUniqueVariables pevs
  checkNoDefinedVariables pevs
  rows <-
    mapM
      ( \item ->
          freshRow item >>= \x -> pure (item, x)
      )
      pevs
  context <- gets translationContext
  let newNameContext = HistoryMap.pushChanges rows (contextByName context)
  let newIdContext =
        foldr
          ( \(_, row) dic ->
              Map.insert
                (rowExpressionVariable row)
                row
                dic
          )
          (contextById context)
          rows
  modify $ \s ->
    s
      { translationContext =
          ContextC
            { contextByName =
                newNameContext
            , contextById = newIdContext
            }
      }
  pure ((rowExpressionVariable <<< snd) <$> rows)


{- | Get the information of a Expression variable
if not found it raises a error
-}
lookupVariable
  :: ( Error TranslationError :> es
     , State TranslationState :> es
     )
  => ParserExpressionVariable
  -> Eff es Row
lookupVariable parserVar = do
  nameContext <- gets (contextByName <<< translationContext)
  case HistoryMap.lookup parserVar nameContext of
    Just row -> pure row
    Nothing -> throwError (UnboundedVariable parserVar)


freshExpressionVar
  :: State TranslationState :> es
  => Eff es InferenceExpressionVar
freshExpressionVar = do
  modify $ \s -> s {nextExpressionVarId = nextExpressionVarId s + 1}
  InferenceExpressionVarC <$> gets nextExpressionVarId


{- | Generate a new type variable that is tied to some
expression variable.
-}
freshTypeVar
  :: State TranslationState :> es
  => Eff es InferenceTypeVar
freshTypeVar = do
  modify $ \s -> s {nextTypeVarId = nextTypeVarId s + 1}
  RealExpressionVar <$> gets nextTypeVarId


freshRow
  :: State TranslationState :> es
  => ParserExpressionVariable
  -> Eff es Row
freshRow pev = do
  newEVar <- freshExpressionVar
  newTVar <- freshTypeVar
  pure $
    RowC
      { rowParserVariable = pev
      , rowExpressionVariable = newEVar
      , rowTypeVariable = newTVar
      , rowExpression = Nothing
      }


cleanupParameters
  :: ( Error TranslationError :> es
     , State TranslationState :> es
     )
  => Eff es ()
cleanupParameters = modify $ \s ->
  let context = translationContext s
      byName = contextByName context
      newByName = HistoryMap.popChanges byName
   in s {translationContext = context {contextByName = newByName}}


cleanupLetNames
  :: ( Error TranslationError :> es
     , State TranslationState :> es
     )
  => [(ParserExpressionVariable, InferenceExpression)]
  -> Eff es ()
cleanupLetNames defs = do
  mapM_ (uncurry addExpressionToRow) defs
  cleanupParameters


transformType
  :: ( Error TranslationError :> es
     , Writer [TranslationWarning] :> es
     , State TranslationState :> es
     )
  => ParserType
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
    TypeVar _ -> (pure <<< TypeVar <<< RealExpressionVar) (-1)


transformLetDefinition
  :: ( Error TranslationError :> es
     , Writer [TranslationWarning] :> es
     , State TranslationState :> es
     )
  => LetDefinition ParserExpressionVariable ParserTypeVariable
  -> Eff es (LetDefinition InferenceExpressionVar InferenceTypeVar)
transformLetDefinition
  LetDefinitionC
    { letName =
      _letName
    , letDefinition = _letDefinition
    } = do
    -- We assume the main let transform already registered
    -- the name!
    newName <- rowExpressionVariable <$> lookupVariable _letName
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
     )
  => ParserExpression
  -> Eff es InferenceExpression
transform expr =
  case expr of
    LiteralExpression l -> pure $ LiteralExpression l
    Variable v -> do
      value <- lookupVariable v
      pure $ Variable (rowExpressionVariable value)
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
| It doesn't update the scope context (contextByName).
-}
addExpressionToRow
  :: ( Error TranslationError :> es
     , State TranslationState :> es
     )
  => ParserExpressionVariable
  -> InferenceExpression
  -> Eff es ()
addExpressionToRow pev ie = do
  row <- lookupVariable pev
  context <- gets translationContext
  let updatedRow = row {rowExpression = Just ie}
  let updatedMap =
        Map.insert
          (rowExpressionVariable row)
          updatedRow
          (contextById context)
  modify $ \s ->
    s
      { translationContext =
          context {contextById = updatedMap}
      }


buildInferenceContextForItem
  :: ( Error TranslationError :> es
     , Writer [TranslationWarning] :> es
     , State TranslationState :> es
     )
  => ParserTopItem
  -> Eff es ()
buildInferenceContextForItem item = do
  let name = ParserNamedVariable $ topItemName item
  let body = topItemBody item
  newBody <- transform body
  addExpressionToRow name newBody


{- | Use it to add a lot of definitions at the
beginning of context. Don't use later!
-}
buildInferenceContext
  :: ( Error TranslationError :> es
     , Writer
        [TranslationWarning]
        :> es
     , State TranslationState :> es
     )
  => [ParserTopItem]
  -> Eff es (Map InferenceExpressionVar Row)
buildInferenceContext items = do
  tell [Warn]
  _ <-
    registerEmpty $
      (ParserNamedVariable <<< topItemName) <$> items
  mapM_ buildInferenceContextForItem items
  (contextById <<< translationContext) <$> get
