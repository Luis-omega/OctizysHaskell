{-# LANGUAGE DataKinds #-}
module Octizys.Effects.SymbolResolution.Interpreter
  ( SourceInfo (SourceInfo', span, preComments, afterComment)
  , runSymbolResolution
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Effectful (Eff, (:>))
import Effectful.State.Static.Local (State, modify)
import Octizys.Cst.Expression
  ( ExpressionVariableId
  , freshExpressionVariableId
  )
import Octizys.Cst.InfoId (InfoId)
import Octizys.Cst.Span (Span)
import Octizys.Cst.Type (TypeVariableId, freshTypeVariableId)
import Octizys.Effects.Generator.Effect (Generator, generate)
import Octizys.Effects.Parser.Combinators (errorMessage)
import Octizys.Effects.Parser.Effect (Parser)
import Octizys.Effects.SymbolResolution.Effect
  ( SymbolResolution(CreateExpressionVariable
  , CreateInformation
  , CreateTypeVariable
                    )
  )
import Octizys.Cst.Comment (Comment)
import Effectful.Dispatch.Dynamic (interpret)

data SourceInfo = SourceInfo'
  { 
    -- | The location in the source of the object.
    span :: Span
    -- | Commentaries that happened before of the object.
  , preComments :: [Comment]
    -- | A line commentarie that may happened after (but in same line) the 
    -- object.
  , afterComment :: Maybe Comment
  }
  deriving (Show, Eq, Ord)


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
  { 
    -- | The names is present only if it was in the source file
    -- otherwise this is a variable automatically generated.
    name :: Maybe Text
    -- | Every type variable has a unique `TypeVariableId` associated.
  , variableId :: TypeVariableId
    -- | For a variable that we see before we know where is 
    -- located. This should be updated later.
  , definitionSpan :: Maybe Span
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
  -- | This is generated automatically for every variable expression.
  , variableId :: ExpressionVariableId
  -- | This is a maybe since we can find a undeclared (syntactically) 
  -- variable and we may need to add the definition place later.
  , definitionSpan :: Maybe Span
  -- | This is generated automatically for every variable expression.
  , typeId :: TypeVariableId
  }
  deriving (Show, Eq, Ord)


registerExpressionVariable
  :: State (Map ExpressionVariableId SourceExpressionVariableInfo) :> es
  => State (Map TypeVariableId SourceTypeVariableInfo) :> es
  => Generator ExpressionVariableId :> es
  => Generator TypeVariableId :> es
  => Text
  -> Maybe Span
  -> Eff es ExpressionVariableId
registerExpressionVariable name definitionSpan = do
  variableId <- freshExpressionVariableId
  -- Type variables don't have name yet as user can't create them!
  typeId <- registerTypeVariable Nothing definitionSpan
  modify $ \s -> Map.insert variableId (SourceExpressionVariableInfo' {..}) s
  pure variableId


runSymbolResolution
  :: State (Map ExpressionVariableId SourceExpressionVariableInfo) :> es
  => State (Map TypeVariableId SourceTypeVariableInfo) :> es
  => State (Map InfoId SourceInfo) :> es
  => Generator ExpressionVariableId :> es
  => Generator TypeVariableId :> es
  => Generator SourceInfo
    :> es
  => Eff
        (SymbolResolution : es)
        ExpressionVariableId
  -> Eff es ExpressionVariableId
runSymbolResolution = interpret $ \ _ x -> 
  case x of 
    CreateExpressionVariable name maybeSpan -> do 
      undefined 
    _ -> undefined


