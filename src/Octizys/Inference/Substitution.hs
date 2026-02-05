module Octizys.Inference.Substitution
  ( Substitution (Substitution)
  , empty
  , singleton
  , applyToConstraint
  , applyToMonoType
  , applyToType
  , compose
  ) where

import qualified Data.Map as Map
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error)
import Prettyprinter (Pretty (pretty), line, (<+>))

import Data.Aeson (ToJSON)
import qualified Data.Bifunctor
import Data.Map (Map)
import qualified Data.Maybe
import GHC.Generics (Generic, Generically (..))
import Octizys.Ast.Type
  ( InferenceVariable (RealTypeVariable)
  , MonoType (..)
  , Scheme
  , Type (..)
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


-- verifyNotRecursive
--   :: Error String :> es
--   => Log :> es
--   => InferenceVariable
--   -> Type InferenceVariable
--   -> Eff es Substitution
-- verifyNotRecursive s@(RealTypeVariable sId) ty =
--   if hasTypeVar s ty
--     then
--       let
--         errorMsg =
--           pretty @String "Error, variable"
--             <+> pretty s
--             <+> pretty @String "is inside"
--             <+> pretty ty
--        in
--         do
--           Log.error
--             "Verification of recursivity on type"
--             [ field "symbol" s
--             , field "type" ty
--             ]
--           throwDocError errorMsg
--     else pure (singleton sId ty)
-- verifyNotRecursive _ ty =

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


-- applySubstitutionToParameter
--   :: Substitution
--   -> Parameter
--   -> Parameter
-- applySubstitutionToParameter s p =
--   let
--     paramType = applySubstitutionToType s <$> getParameterType p
--    in
--     Parameter (getParameterSymbol p) paramType
--
--
-- applySubstitutionToDefinition
--   :: Substitution
--   -> Definition
--   -> Definition
-- applySubstitutionToDefinition s d =
--   let
--     newAnnotation = applySubstitutionToType s <$> d.annotation
--     newExpression = applySubstitutionToExpression s d.value
--    in
--     Definition d.name newAnnotation newExpression
--
--
-- applySubstitutionToExpression
--   :: Substitution
--   -> Expression
--   -> Expression
-- applySubstitutionToExpression s expr =
--   case expr of
--     IntLiteral _ -> expr
--     BoolLiteral _ -> expr
--     AstE.Variable _ -> expr
--     Function param result ->
--       let
--         newParam = applySubstitutionToParameter s param
--         newResult = applySubstitutionToExpression s result
--        in
--         Function newParam newResult
--     Application f arg ->
--       let
--         newF = applySubstitutionToExpression s f
--         newArg = applySubstitutionToExpression s arg
--        in
--         Application newF newArg
--     Record fs -> Record $ Map.map (applySubstitutionToExpression s) fs
--     Selection expre name ->
--       let
--         newExpr = applySubstitutionToExpression s expre
--        in
--         Selection newExpr name
--     Let defs result ->
--       let
--         newDefs = applySubstitutionToDefinition s <$> defs
--         newResult = applySubstitutionToExpression s result
--        in
--         Let newDefs newResult
--     Annotation ty expre ->
--       let
--         newTy = applySubstitutionToType s ty
--         newExpr = applySubstitutionToExpression s expre
--        in
--         Annotation newTy newExpr

compose
  :: Substitution
  -> Substitution
  -> Substitution
compose (Substitution itemsL) s2@(Substitution itemsR) =
  Substitution $
    Map.map (applyToMonoType s2) itemsL
      `Map.union` itemsR
