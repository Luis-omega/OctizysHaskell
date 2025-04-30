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
import Octizys.Cst.Comment (Comment)
import Octizys.Cst.Expression (ExpressionVariableId)
import Octizys.Cst.InfoId (InfoId)
import Octizys.Cst.Span (Span)
import Octizys.Cst.Type (TypeVariableId)


data SymbolResolution :: Effect where
  CreateExpressionVariable
    :: Text
    -> Maybe Span
    -> SymbolResolution m (ExpressionVariableId, TypeVariableId)
  CreateTypeVariable
    :: Maybe Text
    -> Maybe Span
    -> SymbolResolution m TypeVariableId
  CreateInformation
    :: Span
    -> [Comment]
    -> Maybe Comment
    -> SymbolResolution m InfoId


$(makeEffect ''SymbolResolution)

