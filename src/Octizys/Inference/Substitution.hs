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

import Data.Aeson (ToJSON)
import qualified Data.Map as Map
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Prettyprinter (Pretty (pretty), (<+>))

import Data.Map (Map)
import qualified Data.Maybe
import Data.Text (Text)
import GHC.Generics (Generic, Generically (..))

import qualified Octizys.Ast.Expression as AstE
import Octizys.Ast.Type (Type (..))
import Octizys.Ast.Type.Basics
  ( InferenceVariable (ErrorVariable, RealTypeVariable)
  , TypeVariable (TypeVariable')
  , inferenceVarToId
  )
import Octizys.Ast.Type.MonoType
  ( Arrow (Arrow')
  , MonoType (MonoArrow, MonoValue, MonoVariable)
  )
import Octizys.Ast.Type.Scheme (Scheme)
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
    MonoValue _ -> ty
    MonoArrow (Arrow' start remain) ->
      MonoArrow $
        Arrow'
          (applyToMonoType s start)
          (applyToMonoType s <$> remain)
    MonoVariable v ->
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


class ApplyMap a ti tv where
  apply
    :: Error Text :> es
    => Map TypeVariableId (MonoType tv)
    -> a ti
    -> Eff es (a tv)


instance ApplyMap MonoType ti tv => ApplyMap Arrow ti tv where
  apply s (Arrow' start remain) = do
    newStart <- apply s start
    newRemain <- mapM (apply s) remain
    pure $
      Arrow'
        newStart
        newRemain


instance ApplyMap MonoType InferenceVariable tv where
  apply s ty =
    case ty of
      MonoValue v -> pure $ MonoValue v
      MonoArrow (Arrow' start remain) -> do
        newStart <- apply s start
        newRemain <- mapM (apply s) remain
        pure $
          MonoArrow $
            Arrow'
              newStart
              newRemain
      MonoVariable v ->
        case v of
          RealTypeVariable vId ->
            case Map.lookup vId s of
              Just value -> pure value
              Nothing ->
                Format.throwDocError
                  (Format.text "[Error] Couldn't find type variable with id " <> pretty vId)
          ErrorVariable msg ->
            throwError msg


instance ApplyMap Scheme ti tv where
  apply _ _ = undefined


instance ApplyMap MonoType ti tv => ApplyMap Type ti tv where
  apply s (TMono t) = TMono <$> apply s t
  apply s (TPoly t) = from <$> apply s t


instance ApplyMap MonoType ti tv => ApplyMap AstE.Value ti tv where
  apply s v =
    case v of
      AstE.VInt (AstE.ValueInt' {value}) ->
        pure (AstE.VInt (AstE.ValueInt' {value}))
      AstE.VBool (AstE.ValueBool' {value}) -> do
        pure (AstE.VBool (AstE.ValueBool' {value}))
      AstE.VFunction (AstE.Function' {parameters, body, inferType}) -> do
        newType <- apply s inferType
        newParams <-
          mapM
            ( \(eid, mt) -> do
                newMt <- apply s mt
                pure (eid, newMt)
            )
            parameters
        newBody <- apply s body
        pure $
          AstE.VFunction $
            AstE.Function'
              { parameters = newParams
              , body =
                  newBody
              , inferType = newType
              }


instance ApplyMap MonoType ti tv => ApplyMap AstE.Definition ti tv where
  apply s AstE.Definition' {definition, inferType, ..} = do
    newType <- apply s inferType
    newExpr <- apply s definition
    pure AstE.Definition' {definition = newExpr, inferType = newType, ..}


instance ApplyMap MonoType ti tv => ApplyMap AstE.Expression ti tv where
  apply s expr =
    case expr of
      AstE.EVariable (AstE.Variable' {name, inferType}) -> do
        newType <- apply s inferType
        pure $ AstE.EVariable (AstE.Variable' {name, inferType = newType})
      AstE.EValue v -> AstE.EValue <$> apply s v
      AstE.EApplication
        (AstE.Application' {inferType, function, argument}) -> do
          newType <- apply s inferType
          newFunc <- apply s function
          newArg <- apply s argument
          pure $
            AstE.EApplication $
              AstE.Application'
                { inferType = newType
                , function = newFunc
                , argument = newArg
                }
      AstE.EIf (AstE.If' {condition, ifTrue, ifFalse, inferType}) -> do
        newType <- apply s inferType
        newCond <- apply s condition
        newTrue <- apply s ifTrue
        newFalse <- apply s ifFalse
        pure $
          AstE.EIf $
            AstE.If'
              { condition = newCond
              , ifTrue = newTrue
              , ifFalse = newFalse
              , inferType = newType
              }
      AstE.ELet (AstE.Let' {definitions, expression, inferType}) -> do
        newType <- apply s inferType
        newDefinitions <- mapM (apply s) definitions
        newExpr <- apply s expression
        pure $
          AstE.ELet $
            AstE.Let'
              { definitions = newDefinitions
              , expression = newExpr
              , inferType = newType
              }
      AstE.EAnnotation (AstE.Annotation' {expression, _type, inferType}) -> do
        newType <- apply s inferType
        newExpr <- apply s expression
        newTypeAnn <- apply s _type
        pure $
          AstE.EAnnotation $
            AstE.Annotation'
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
    MonoValue v -> pure $ MonoValue v
    MonoArrow (Arrow' start remain) -> do
      newStart <- finalizeMonoType start
      newRemain <- mapM finalizeMonoType remain
      pure $
        MonoArrow $
          Arrow'
            newStart
            newRemain
    MonoVariable v ->
      case v of
        RealTypeVariable vId ->
          pure $ MonoVariable $ TypeVariable' vId
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
