{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Octizys.Effects.SymbolResolution.Interpreter
  ( SourceInfo (SourceInfo', span, preComments, afterComment)
  , runSymbolResolution
  , runSymbolResolutionWithState
  , SourceExpressionVariableInfo
  , SourceTypeVariableInfo
  , SymbolResolutionState
    ( SymbolResolutionState'
    , genVarType
    , genVarExp
    , genInfoId
    , expVarTable
    , typeVarTable
    , infoTable
    )
  ,initialSymbolResolutionState
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Effectful (Eff, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Local (State, modify, runState)
import Octizys.Cst.Comment (Comment)
import Octizys.Cst.Expression
  ( ExpressionVariableId (unExpressionVariableId)
  , freshExpressionVariableId
  )
import Octizys.Cst.InfoId (InfoId (unInfoId))
import Octizys.Cst.Span (Span)
import Octizys.Cst.Type
  ( TypeVariableId (unTypeVariableId)
  , freshTypeVariableId
  )
import Octizys.Cst.VariableId (unVariableId)
import Octizys.Effects.Generator.Effect (Generator, IntGenerator, generate)
import Octizys.Effects.Generator.Interpreter
  ( IntGeneratorState
  , runGeneratorFull
  )
import Octizys.Effects.SymbolResolution.Effect
  ( SymbolResolution
      ( CreateExpressionVariable
      , CreateInformation
      , CreateTypeVariable
      )
  )
import Octizys.Pretty.Comment (prettyComment)
import Prettyprinter (Pretty (pretty), (<+>))
import qualified Prettyprinter as Pretty
import Prelude hiding (span)


data SourceInfo = SourceInfo'
  { span :: Span
  -- ^ The location in the source of the object.
  , preComments :: [Comment]
  -- ^ Commentaries that happened before of the object.
  , afterComment :: Maybe Comment
  -- ^ A line commentarie that may happened after (but in same line) the
  -- object.
  }
  deriving (Show, Eq, Ord)


instance Pretty SourceInfo where
  pretty
    SourceInfo'
      { span = _span
      , preComments = _preComments
      , afterComment = _afterComment
      } =
      "Span:"
        <+> pretty (show _span)
        <+> Pretty.vsep (prettyComment <$> _preComments)
        <+> Pretty.hardline
        <+> maybe mempty prettyComment _afterComment


registerInfo
  :: State (Map InfoId SourceInfo) :> es
  => Generator InfoId :> es
  => Span
  -> [Comment]
  -> Maybe Comment
  -> Eff es InfoId
registerInfo span preComments afterComment = do
  newId <- generate
  modify $ \s -> Map.insert newId (SourceInfo' {..}) s
  pure newId


data SourceTypeVariableInfo = SourceTypeVariableInfo'
  { name :: Maybe Text
  -- ^ The names is present only if it was in the source file
  -- otherwise this is a variable automatically generated.
  , variableId :: TypeVariableId
  -- ^ Every type variable has a unique `TypeVariableId` associated.
  , definitionSpan :: Maybe Span
  -- ^ For a variable that we see before we know where is
  -- located. This should be updated later.
  }
  deriving (Show, Eq, Ord)


registerTypeVariable
  :: State (Map TypeVariableId SourceTypeVariableInfo) :> es
  => Generator TypeVariableId :> es
  => Maybe Text
  -> Maybe Span
  -> Eff es TypeVariableId
registerTypeVariable name definitionSpan = do
  variableId <- freshTypeVariableId
  modify $ \s -> Map.insert variableId (SourceTypeVariableInfo' {..}) s
  pure variableId


data SourceExpressionVariableInfo = SourceExpressionVariableInfo'
  { name :: Text
  , variableId :: ExpressionVariableId
  -- ^ This is generated automatically for every variable expression.
  , definitionSpan :: Maybe Span
  -- ^ This is a maybe since we can find a undeclared (syntactically)
  -- variable and we may need to add the definition place later.
  , typeId :: TypeVariableId
  -- ^ This is generated automatically for every variable expression.
  }
  deriving (Show, Eq, Ord)


registerExpressionVariable
  :: State (Map ExpressionVariableId SourceExpressionVariableInfo) :> es
  => State (Map TypeVariableId SourceTypeVariableInfo) :> es
  => Generator ExpressionVariableId :> es
  => Generator TypeVariableId :> es
  => Text
  -> Maybe Span
  -> Eff es (ExpressionVariableId, TypeVariableId)
registerExpressionVariable name definitionSpan = do
  variableId <- freshExpressionVariableId
  -- Type variables don't have name yet as user can't create them!
  typeId <- registerTypeVariable Nothing definitionSpan
  modify $ \s -> Map.insert variableId (SourceExpressionVariableInfo' {..}) s
  pure (variableId, typeId)


runSymbolResolution
  :: State (Map ExpressionVariableId SourceExpressionVariableInfo) :> es
  => State (Map TypeVariableId SourceTypeVariableInfo) :> es
  => State (Map InfoId SourceInfo) :> es
  => Generator ExpressionVariableId :> es
  => Generator TypeVariableId :> es
  => Generator InfoId
    :> es
  => Eff
      (SymbolResolution : es)
      a
  -> Eff es a
runSymbolResolution = interpret $ \_ x ->
  case x of
    CreateExpressionVariable name maybeSpan ->
      registerExpressionVariable name maybeSpan
    CreateTypeVariable maybeName maybeSpan ->
      registerTypeVariable maybeName maybeSpan
    CreateInformation span preComments afterComment ->
      registerInfo span preComments afterComment


data SymbolResolutionState = SymbolResolutionState'
  { genVarType :: Int
  , genVarExp :: Int
  , genInfoId :: Int
  , expVarTable :: Map ExpressionVariableId SourceExpressionVariableInfo
  , typeVarTable :: Map TypeVariableId SourceTypeVariableInfo
  , infoTable :: Map InfoId SourceInfo
  }


runSymbolResolutionWithState
  :: SymbolResolutionState
  -> Eff
      ( SymbolResolution
          : Generator TypeVariableId
          : IntGenerator TypeVariableId
          : State (IntGeneratorState TypeVariableId)
          : Generator ExpressionVariableId
          : IntGenerator ExpressionVariableId
          : State (IntGeneratorState ExpressionVariableId)
          : Generator InfoId
          : IntGenerator InfoId
          : State (IntGeneratorState InfoId)
          : State (Map ExpressionVariableId SourceExpressionVariableInfo)
          : State (Map TypeVariableId SourceTypeVariableInfo)
          : State (Map InfoId SourceInfo)
          : es
      )
      b
  -> Eff
      es
      (b, SymbolResolutionState)
runSymbolResolutionWithState s action = do
  ( ( ( ( ( (a, nextVarType)
            , nextVarExp
            )
          , nextInfoId
          )
        , expTable
        )
      , typeTable
      )
    , iTable
    ) <-
    runState
      s.infoTable
      ( runState
          s.typeVarTable
          ( runState
              s.expVarTable
              ( runGeneratorFull
                  s.genInfoId
                  ( runGeneratorFull
                      s.genVarExp
                      ( runGeneratorFull
                          s.genVarType
                          (runSymbolResolution action)
                      )
                  )
              )
          )
      )
  pure
    ( a
    , SymbolResolutionState'
        { genVarType = unVariableId $ unTypeVariableId nextVarType
        , genVarExp = unVariableId $ unExpressionVariableId nextVarExp
        , genInfoId = unInfoId nextInfoId
        , expVarTable = expTable
        , typeVarTable = typeTable
        , infoTable = iTable
        }
    )


initialSymbolResolutionState :: SymbolResolutionState
initialSymbolResolutionState =
  SymbolResolutionState'
    { genVarType = 0
    , genVarExp = 0
    , genInfoId = 0
    , expVarTable = mempty
    , typeVarTable = mempty
    , infoTable = mempty
    }

