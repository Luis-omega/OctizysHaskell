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
import Prettyprinter (Pretty (pretty), (<+>))

import Data.Aeson (ToJSON)
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
  , inferenceVarToId
  )
import Octizys.Classes.From (from)
import Octizys.Common.Id
import Octizys.Format.Class (Formattable (format))
import qualified Octizys.Format.Utils as Format
import Octizys.Inference.Constraint (Constraint, modifyTypes)


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


instance Formattable Substitution where
  format configuration s =
    Format.formatMapWith
      configuration
      (\c (x, y) -> format c x <+> pretty '~' <+> format c y)
      (pretty ',')
      s.unSubstitution


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
compose s1@(Substitution itemsL) (Substitution itemsR) =
  Substitution $
    Map.map (applyToMonoType s1) itemsR
      `Map.union` itemsL


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
                Format.throwDocError
                  (Format.text "[Error] Couldn't find type variable with id " <> pretty vId)
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
