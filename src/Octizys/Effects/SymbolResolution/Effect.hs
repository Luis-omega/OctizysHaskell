{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- | Description: This module defines a effect used for
symbol resolution. It can create new identifiers for
both expression and type variables. It can save the
source information and generate information ids.
-}
module Octizys.Effects.SymbolResolution.Effect where

import Data.Text (Text)
import Effectful (Effect)
import Effectful.TH (makeEffect)
import Octizys.Common.Id (ExpressionVariableId, InfoId, TypeVariableId)
import Octizys.Cst.Comment (Comment)
import Octizys.Cst.Span (Span)

import Data.Map (Map)
import Octizys.Common.HistoryMap
import Octizys.Common.Name (Name)
import Octizys.Common.Qualifier (Qualifier)


data SymbolResolutionState = SymbolResolutionState'
  { expVarTable :: Map ExpressionVariableId SourceExpressionVariableInfo
  , typeVarTable :: Map TypeVariableId SourceTypeVariableInfo
  , infoTable :: Map InfoId SourceInfo
  , -- All the definition places associated with a variable name
    expNamesToId :: HistoryMap Text ExpressionVariableId
  , typeNamesToId :: HistoryMap Text TypeVariableId
  }
  deriving (Show)


data SymbolResolution :: Effect where
  -- | While scanning we found a expression variable somewhere.
  -- This is not a binding place, so we don't know where
  -- it was defined.
  FoundExpressionVariable
    :: Text
    -> SymbolResolution m ExpressionVariableId
  -- | While scanning we found a type variable somewhere.
  -- This is not a binding place, so we don't know where
  -- it was defined.
  FoundTypeVariable
    :: Text
    -> SymbolResolution m TypeVariableId
  -- | While scanning we found the definition place of a
  -- variable.
  DefinitionOfExpressionVariable
    :: Text
    -> Span
    -> SymbolResolution m ExpressionVariableId
  -- | While scanning we found the definition place of a
  -- variable.
  DefinitionOfTypeVariable
    :: Text
    -> Span
    -> SymbolResolution m TypeVariableId
  -- | Call this when a definition for a expression variable is
  -- out of bounds
  RemoveExpressionDefinition
    :: ExpressionVariableId
    -> SymbolResolution m ()
  -- | Call this when a definition for a type variable is
  -- out of bounds
  RemoveTypeDefinition
    :: TypeVariableId
    -> SymbolResolution m ()
  -- | Create the source information for some token/variable etc.
  CreateInformation
    :: Span
    -> [Comment]
    -> Maybe Comment
    -> SymbolResolution m InfoId
  GetSymbolResolutionState
    :: SymbolResolution m SymbolResolutionState
  PutSymbolResolutionState
    :: SymbolResolutionState -> SymbolResolution m ()


$(makeEffect ''SymbolResolution)
