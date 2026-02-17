{-# LANGUAGE UndecidableInstances #-}

module Octizys.Inference.Substitution
  ( Substitution (Substitution)
  , empty
  , singleton
  , finalizeSubstitution
  , applyToConstraint
  , applyToMonoType
  , applyToType
  , compose
  , ApplyMap (apply)
  ) where

import qualified Data.Map as Map
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Prettyprinter (Pretty (pretty), line, (<+>))

import Data.Aeson (ToJSON)
import qualified Data.Bifunctor
import Data.Map (Map)
import qualified Data.Maybe
import Data.Text (Text)
import GHC.Generics (Generic, Generically (..))
import qualified Octizys.Ast.Expression as AstE
import Octizys.Ast.Type
  ( InferenceVariable (ErrorVariable, RealTypeVariable)
  , MonoType (..)
  , Scheme
  , Type (..)
  , TypeVariable (TypeVariable')
  , hasTypeVar
  , inferenceVarToId
  )
import Octizys.Classes.From (from)
import Octizys.Common.Format (throwDocError)
import qualified Octizys.Common.Format as Common
import Octizys.Common.Id
import Octizys.Inference.Constraint (Constraint, modifyTypes)
import Octizys.Logging.Effect (Log)
import Octizys.Logging.Entry (field)
import qualified Octizys.Logging.Loggers as Log


newtype Substitution = Substitution
  {unSubstitution :: Map TypeVariableId (MonoType InferenceVariable)}
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically Substitution


empty :: Substitution
empty = Substitution mempty


singleton
  :: TypeVariableId -> MonoType InferenceVariable -> Substitution
singleton tv ty =
  Substitution (Map.singleton tv ty)


instance Pretty Substitution where
  pretty s =
    pretty '('
      <+> line
      <+> Common.prettyItemList
        (Map.toList s.unSubstitution)
        (pretty ',')
        (pretty '~')
      <+> line
      <+> pretty ')'


applyToMonoType
  :: Substitution
  -> MonoType InferenceVariable
  -> MonoType InferenceVariable
applyToMonoType s ty =
  case ty of
    VType _ -> ty
    Arrow start remain ->
      Arrow
        (applyToMonoType s start)
        (applyToMonoType s <$> remain)
    Variable v ->
      case inferenceVarToId v of
        Just vId ->
          Data.Maybe.fromMaybe
            ty
            (Map.lookup vId s.unSubstitution)
        Nothing -> ty


applyToScheme
  :: Substitution
  -> Scheme InferenceVariable
  -> Scheme InferenceVariable
applyToScheme = undefined


applyToType
  :: Substitution
  -> Type InferenceVariable
  -> Type InferenceVariable
applyToType s (TMono t) = from $ applyToMonoType s t
applyToType s (TPoly t) = from $ applyToScheme s t


applyToConstraint
  :: Substitution
  -> Constraint
  -> Constraint
applyToConstraint s c =
  let
    (t1, t2) = from c
    t3 = applyToMonoType s t1
    t4 = applyToMonoType s t2
   in
    modifyTypes t3 t4 c


compose
  :: Substitution
  -> Substitution
  -> Substitution
compose (Substitution itemsL) s2@(Substitution itemsR) =
  Substitution $
    Map.map (applyToMonoType s2) itemsL
      `Map.union` itemsR


class ApplyMap a tv ti where
  apply
    :: Error Text :> es
    => Map TypeVariableId (MonoType tv)
    -> a ti
    -> Eff es (a tv)


instance ApplyMap MonoType tv InferenceVariable where
  apply s ty =
    case ty of
      VType v -> pure $ VType v
      Arrow start remain -> do
        newStart <- apply s start
        newRemain <- mapM (apply s) remain
        pure $
          Arrow
            newStart
            newRemain
      Variable v ->
        case v of
          RealTypeVariable vId ->
            case Map.lookup vId s of
              Just value -> pure value
              Nothing ->
                throwDocError
                  (Common.pText "[Error] Couldn't find type variable with id " <> pretty vId)
          ErrorVariable msg ->
            throwError msg


instance ApplyMap Scheme tv ti where
  apply _ _ = undefined


instance ApplyMap MonoType tv ti => ApplyMap Type tv ti where
  apply s (TMono t) = TMono <$> apply s t
  apply s (TPoly t) = from <$> apply s t


instance ApplyMap MonoType tv ti => ApplyMap AstE.Value tv ti where
  apply s v =
    case v of
      AstE.VInt {intValue, inferType} -> do
        newType <- apply s inferType
        pure AstE.VInt {intValue, inferType = newType}
      AstE.VBool {boolValue, inferType} -> do
        newType <- apply s inferType
        pure AstE.VBool {boolValue, inferType = newType}
      AstE.Function {parameters, body, inferType} -> do
        newType <- apply s inferType
        newParams <-
          mapM
            ( \(eid, mt) -> do
                newMt <- apply s mt
                pure (eid, newMt)
            )
            parameters
        newBody <- apply s body
        pure
          AstE.Function
            { parameters = newParams
            , body =
                newBody
            , inferType = newType
            }


instance ApplyMap MonoType tv ti => ApplyMap AstE.Definition tv ti where
  apply s AstE.Definition' {definition, inferType, ..} = do
    newType <- apply s inferType
    newExpr <- apply s definition
    pure AstE.Definition' {definition = newExpr, inferType = newType, ..}


instance ApplyMap MonoType tv ti => ApplyMap AstE.Expression tv ti where
  apply s expr =
    case expr of
      AstE.Variable {name, inferType} -> do
        newType <- apply s inferType
        pure AstE.Variable {name, inferType = newType}
      AstE.EValue {inferType, value} -> do
        newType <- apply s inferType
        newValue <- apply s value
        pure AstE.EValue {inferType = newType, value = newValue}
      AstE.Application {inferType, applicationFunction, applicationArgument} -> do
        newType <- apply s inferType
        newFunc <- apply s applicationFunction
        newArg <- apply s applicationArgument
        pure
          AstE.Application
            { inferType = newType
            , applicationFunction = newFunc
            , applicationArgument = newArg
            }
      AstE.If {condition, ifTrue, ifFalse, inferType} -> do
        newType <- apply s inferType
        newCond <- apply s condition
        newTrue <- apply s ifTrue
        newFalse <- apply s ifFalse
        pure
          AstE.If
            { condition = newCond
            , ifTrue = newTrue
            , ifFalse = newFalse
            , inferType = newType
            }
      AstE.Let {definitions, expression, inferType} -> do
        newType <- apply s inferType
        newDefinitions <- mapM (apply s) definitions
        newExpr <- apply s expression
        pure
          AstE.Let
            { definitions = newDefinitions
            , expression = newExpr
            , inferType = newType
            }
      AstE.Annotation {expression, _type, inferType} -> do
        newType <- apply s inferType
        newExpr <- apply s expression
        newTypeAnn <- apply s _type
        pure
          AstE.Annotation
            { expression = newExpr
            , _type = newTypeAnn
            , inferType =
                newType
            }


finalizeMonoType
  :: Error Text :> es
  => MonoType InferenceVariable
  -> Eff es (MonoType TypeVariable)
finalizeMonoType ty =
  case ty of
    VType v -> pure $ VType v
    Arrow start remain -> do
      newStart <- finalizeMonoType start
      newRemain <- mapM finalizeMonoType remain
      pure $
        Arrow
          newStart
          newRemain
    Variable v ->
      case v of
        RealTypeVariable vId ->
          pure $ Variable $ TypeVariable' vId
        ErrorVariable msg ->
          throwError msg


finalizeSubstitution
  :: Error Text :> es
  => Substitution
  -> Eff es (Map TypeVariableId (MonoType TypeVariable))
finalizeSubstitution (Substitution s) =
  Map.traverseWithKey
    (\_ y -> finalizeMonoType y)
    s
