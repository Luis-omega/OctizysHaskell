{-# LANGUAGE GADTs #-}

module Octizys.Inference.Constraint
  ( Constraint
  , makeConstraint
  , ConstraintInfo
  , makeConstraintInfo
  , ConstraintReason (..)
  , makeConstraintFromParent
  , getAst
  , getCst
  , getReason
  , getConstraintId
  , getLeftType
  , getRightType
  , asTuple
  , ConstraintId
  , generateConstraintId
  , modifyTypes
  ) where

import Control.Arrow ((<<<))
import Data.Aeson (ToJSON)
import Data.Text (Text)
import Effectful (Eff, (:>))
import Effectful.State.Static.Local (State, get, put)
import GHC.Generics (Generic, Generically (..))
import qualified Octizys.Ast.Node as Ast
import Octizys.Ast.Type (InferenceVariable)
import qualified Octizys.Ast.Type as AstT
import Octizys.Classes.From
import Octizys.Common.Id (ExpressionVariableId)
import qualified Octizys.FrontEnd.Cst.Node as Cst
import Octizys.FrontEnd.Cst.Type
  ( TypeVariableId
  )
import Prettyprinter (Pretty (pretty))
import Prelude hiding (lookup)


data ConstraintReason
  = -- | The condition inside an if should be a boolean type
    IfConditionShouldBeBool
  | -- | The cases inside a if should have the same type.
    IfCasesShouldMatch
  | ApplicationShouldBeOnArrows
  | ArgumentShouldBeOfDomainType
  | TypeAnnotation
  | -- | A parameter was found with type annotation
    ParameterTypeAnnotation
  | -- | A variable was defined without arguments, we must be sure
    -- that it's body has the same type as the one assigned to the name.
    -- `let f = body` => `type f ~ type body`
    DefinitionVariableAndBodyShouldBeEqual
  | -- | f : Type
    DefinitionTypeAnnotation
  | -- | f : x , y , Type
    DefinitionTypeAnnotationWithArgs
  | -- | We already know the type of a variable, it can be
    -- because is a builtin or we are in the repl and we solved
    -- it previously.
    IsKnowType
  deriving (Show, Eq, Ord)


newtype ConstraintId where
  ConstraintId' :: {getConstraintIdAsInt :: Int} -> ConstraintId
  deriving (Eq, Ord, Show) via ConstraintId
  deriving (Generic)
  deriving (ToJSON) via Generically ConstraintId


instance Pretty ConstraintId where
  pretty = pretty <<< getConstraintIdAsInt


generateConstraintId
  :: State ConstraintId :> es
  => Eff es ConstraintId
generateConstraintId = do
  current@(ConstraintId' internalInt) <- get
  put (ConstraintId' (internalInt + 1))
  pure current


instance Pretty ConstraintReason where
  pretty IfConditionShouldBeBool = "If condition must be of type bool."
  pretty IfCasesShouldMatch = "If branch cases must have same type."
  pretty ApplicationShouldBeOnArrows = "Application must be only done by functions."
  pretty ArgumentShouldBeOfDomainType = "Argument of a function should have it's domain type."
  pretty TypeAnnotation = "Type annotation."
  pretty ParameterTypeAnnotation = "Type annotation on parameter."
  pretty DefinitionVariableAndBodyShouldBeEqual = "Inferred type of variable and it's definition must match."
  pretty DefinitionTypeAnnotation = "Type annotation on variable definition."
  pretty DefinitionTypeAnnotationWithArgs = "Type annotation on function definition."
  pretty IsKnowType = "Type was already inferred previously or is an import or built in."


data ConstraintInfo = ConstraintInfo'
  { reason :: ConstraintReason
  , cst :: Cst.Node ExpressionVariableId TypeVariableId
  -- ^ The original node that instigated
  -- the constraint.
  , ast :: Ast.Node InferenceVariable
  -- ^ The node translated to ast, it contains
  -- all the info to report errors!
  , --  | If this constraint is derivation of another one
    parent :: Maybe ConstraintId
  , dependedOn :: [ConstraintId]
  , constraintId :: ConstraintId
  }
  deriving (Show, Ord, Eq)


instance Pretty ConstraintInfo where
  pretty c = pretty (c.reason, c.constraintId, c.ast)


makeConstraintInfo
  :: State ConstraintId :> es
  => ConstraintReason
  -> Cst.Node ExpressionVariableId TypeVariableId
  -> Ast.Node InferenceVariable
  -> Maybe Constraint
  -> [Constraint]
  -> Eff es ConstraintInfo
makeConstraintInfo reason cst ast maybeParent dependents = do
  newId <- generateConstraintId
  let
    maybeParentId = getConstraintId <$> maybeParent
    dependentsIds = getConstraintId <$> dependents
  pure $ ConstraintInfo' reason cst ast maybeParentId dependentsIds newId


data Constraint = Constraint'
  { constraintType1 :: AstT.MonoType InferenceVariable
  , constraintType2 :: AstT.MonoType InferenceVariable
  , constraintInfo :: ConstraintInfo
  }
  deriving (Show, Ord, Eq)


instance Pretty Constraint where
  pretty c =
    pretty @Text "Constraint["
      <> pretty (c.constraintType1, c.constraintType2, c.constraintInfo)
      <> pretty @Text "]"


makeConstraint
  :: AstT.MonoType InferenceVariable
  -> AstT.MonoType InferenceVariable
  -> ConstraintInfo
  -> Constraint
makeConstraint = Constraint'


makeConstraintFromParent
  :: State ConstraintId :> es
  => AstT.MonoType InferenceVariable
  -> AstT.MonoType InferenceVariable
  -> Constraint
  -> Eff es Constraint
makeConstraintFromParent ast1 ast2 parent = do
  newInfo <-
    makeConstraintInfo
      parent.constraintInfo.reason
      parent.constraintInfo.cst
      parent.constraintInfo.ast
      (Just parent)
      []
  pure $ makeConstraint ast1 ast2 newInfo


getCst
  :: Constraint -> Cst.Node ExpressionVariableId TypeVariableId
getCst c = c.constraintInfo.cst


getAst :: Constraint -> Ast.Node InferenceVariable
getAst c = c.constraintInfo.ast


getReason :: Constraint -> ConstraintReason
getReason c = c.constraintInfo.reason


getConstraintId :: Constraint -> ConstraintId
getConstraintId c = c.constraintInfo.constraintId


getLeftType :: Constraint -> AstT.MonoType InferenceVariable
getLeftType (Constraint' l _ _) = l


getRightType :: Constraint -> AstT.MonoType InferenceVariable
getRightType (Constraint' _ r _) = r


asTuple
  :: Constraint
  -> (AstT.MonoType InferenceVariable, AstT.MonoType InferenceVariable)
asTuple (Constraint' l r _) = (l, r)


instance
  From
    (AstT.MonoType InferenceVariable, AstT.MonoType InferenceVariable)
    Constraint
  where
  from = asTuple


modifyTypes
  :: AstT.MonoType InferenceVariable
  -> AstT.MonoType InferenceVariable
  -> Constraint
  -> Constraint
modifyTypes t1 t2 (Constraint' _ _ info) = Constraint' t1 t2 info
