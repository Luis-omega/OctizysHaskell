{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Octizys.Ast.Type where

import Control.Arrow ((<<<))
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty ((:|)), cons)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Set (singleton)
import qualified Data.Set as Set
import Octizys.Classes.FreeVariables (FreeVariables (freeVariables))
import Octizys.Classes.From (From (from))
import Octizys.FrontEnd.Cst.Type (TypeVariableId)

import Data.Aeson (ToJSON, ToJSONKey)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe
import Data.Text (Text)
import Effectful (Eff, (:>))
import GHC.Generics (Generic, Generically (..))
import Octizys.Common.Format (defaultIndentationSpaces)
import Octizys.Common.Id (GenerateFromInt (generateFromInt))
import Octizys.Effects.IdGenerator.Effect (IdGenerator, generateId)
import Prettyprinter (Pretty, pretty)
import qualified Prettyprinter as Pretty


class TypeEq a where
  typeEq :: a -> a -> Bool


class NormalizeType a where
  normalize :: a -> a


data TypeValue = BoolType | IntType
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically TypeValue


instance FreeVariables TypeVariableId TypeValue where
  freeVariables _ = mempty


instance Pretty TypeValue where
  pretty BoolType = pretty @Text "Bool"
  pretty IntType = pretty @Text "Int"


instance TypeEq TypeValue where
  typeEq = (==)


data InferenceVariable
  = ErrorVariable Text
  | RealTypeVariable TypeVariableId
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically InferenceVariable


instance ToJSONKey InferenceVariable


instance FreeVariables TypeVariableId InferenceVariable where
  freeVariables (RealTypeVariable tid) = singleton tid
  freeVariables (ErrorVariable _) = mempty


instance Pretty InferenceVariable where
  pretty (ErrorVariable _) = pretty @Text "ErrorVariable"
  pretty (RealTypeVariable vid) = pretty vid


instance From InferenceVariable TypeVariableId where
  from = RealTypeVariable


instance GenerateFromInt InferenceVariable where
  generateFromInt sc oi i = RealTypeVariable $ generateFromInt sc oi i


instance TypeEq InferenceVariable where
  typeEq (RealTypeVariable x) (RealTypeVariable y) = x == y
  typeEq _ _ = False


newtype TypeVariable = TypeVariable' {unTypeVariable :: TypeVariableId}
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically TypeVariable


instance Pretty TypeVariable where
  pretty (TypeVariable' vid) = pretty vid


instance FreeVariables TypeVariableId TypeVariable where
  freeVariables v = singleton v.unTypeVariable


instance GenerateFromInt TypeVariable where
  generateFromInt sc oi i = TypeVariable' $ generateFromInt sc oi i


instance TypeEq TypeVariable where
  typeEq = (==)


inferenceVarToId :: InferenceVariable -> Maybe TypeVariableId
inferenceVarToId (RealTypeVariable i) = pure i
inferenceVarToId (ErrorVariable _) = Nothing


data MonoType var
  = VType {value :: TypeValue}
  | Arrow
      { start :: MonoType var
      , remain :: NonEmpty (MonoType var)
      }
  | Variable var
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (MonoType var)


instance From (MonoType var) TypeValue where
  from = VType


instance From outVar inVar => From (MonoType outVar) (MonoType inVar) where
  from VType {value} = VType {value}
  from Arrow {start, remain} = Arrow {start = from start, remain = from <$> remain}
  from (Variable var) = Variable (from var)


instance NormalizeType (MonoType var) where
  normalize (Arrow start1 remain1) =
    let
      start2 = normalize start1
      remain2 = normalize <$> remain1
     in
      flattenArrows start2 (NonEmpty.toList remain2)
    where
      flattenArrows :: MonoType var -> [MonoType var] -> MonoType var
      flattenArrows initType [] = initType
      flattenArrows initType (x : remain) =
        case flattenArrows x remain of
          Arrow newDom newRange ->
            Arrow initType (NonEmpty.cons newDom newRange)
          _ -> Arrow initType (x :| remain)
  normalize x = x


instance TypeEq a => TypeEq [a] where
  typeEq [] [] = True
  typeEq (x : xs) (y : ys) = typeEq x y && typeEq xs ys
  typeEq _ _ = False


instance TypeEq a => TypeEq (NonEmpty a) where
  typeEq x y = typeEq (NonEmpty.toList x) (NonEmpty.toList y)


instance TypeEq var => TypeEq (MonoType var) where
  typeEq x y =
    case (normalize x, normalize y) of
      (VType v1, VType v2) -> typeEq v1 v2
      (Arrow start1 remain1, Arrow start2 remain2) ->
        typeEq start1 start2 && typeEq remain1 remain2
      (Variable v1, Variable v2) -> typeEq v1 v2
      (_, _) -> False


needsParentsInArrow :: MonoType var -> Bool
needsParentsInArrow t =
  case t of
    VType {} -> False
    Arrow {} -> True
    Variable {} -> False


hasTypeVarMono
  :: Eq var
  => var
  -> MonoType var
  -> Bool
hasTypeVarMono _ (VType _) = False
hasTypeVarMono v (Arrow t1 ts) =
  hasTypeVarMono v t1 || any (hasTypeVarMono v) ts
hasTypeVarMono v (Variable v2) = v == v2


arrowFromArgsAndOutput
  :: NonEmpty (MonoType var)
  -> MonoType var
  -> MonoType var
arrowFromArgsAndOutput = undefined


replaceMonoVars
  :: Map InferenceVariable (MonoType InferenceVariable)
  -> MonoType InferenceVariable
  -> MonoType InferenceVariable
replaceMonoVars _ t@(VType _) = t
replaceMonoVars s (Arrow t1 t2) =
  Arrow
    (replaceMonoVars s t1)
    (replaceMonoVars s <$> t2)
replaceMonoVars s t@(Variable v) =
  Data.Maybe.fromMaybe t (Map.lookup v s)


replaceMonoTypeId
  :: Map TypeVariableId TypeVariableId
  -> MonoType TypeVariable
  -> MonoType TypeVariable
replaceMonoTypeId _ t@(VType _) = t
replaceMonoTypeId s (Arrow t1 t2) =
  Arrow
    (replaceMonoTypeId s t1)
    (replaceMonoTypeId s <$> t2)
replaceMonoTypeId s t@(Variable v) =
  case Map.lookup (unTypeVariable v) s of
    Just newId -> Variable (TypeVariable' newId)
    Nothing -> t


instance Pretty var => Pretty (MonoType var) where
  pretty VType {value} = pretty value
  pretty Arrow {start, remain} =
    (Pretty.group <<< Pretty.indent defaultIndentationSpaces)
      ( Pretty.line'
          <> Pretty.concatWith
            (\l r -> l <> Pretty.line <> pretty @Text "->" <> r)
            ( prettyArg
                <$> cons start remain
            )
      )
    where
      prettyArg ty =
        if needsParentsInArrow ty
          then Pretty.parens (pretty ty)
          else pretty ty
  pretty (Variable v) = pretty v


instance
  (Eq var, Ord var)
  => FreeVariables var (MonoType var)
  where
  freeVariables VType {} = mempty
  freeVariables Arrow {start, remain} =
    foldl'
      (<>)
      (freeVariables start)
      (freeVariables <$> remain)
  freeVariables (Variable v) = Set.singleton v


data Scheme var = Scheme'
  { arguments :: NonEmpty TypeVariableId
  , body :: MonoType var
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (Scheme var)


instance Pretty var => Pretty (Scheme var) where
  pretty (Scheme' {arguments, body}) =
    pretty @Text "forall"
      <> Pretty.indent
        defaultIndentationSpaces
        ( Pretty.line
            <> Pretty.fillSep
              ( Pretty.punctuate
                  (pretty ',')
                  (pretty <$> NonEmpty.toList arguments)
              )
        )
      <> Pretty.line
      <> pretty '.'
      <> Pretty.indent
        defaultIndentationSpaces
        ( pretty body
        )


instance
  (Eq var, Ord var, From var TypeVariableId)
  => FreeVariables var (Scheme var)
  where
  freeVariables s =
    Set.difference
      (freeVariables s.body)
      (Set.fromList (NonEmpty.toList (from <$> s.arguments)))


pairSchemeVars
  :: NonEmpty TypeVariableId
  -> NonEmpty TypeVariableId
  -> Either () (Map TypeVariableId TypeVariableId)
pairSchemeVars x y =
  case go (NonEmpty.toList x) (NonEmpty.toList y) of
    Right xs -> Right $ Map.fromList xs
    Left _ -> Left ()
  where
    go [] [] = Right mempty
    go (x1 : xs) (y1 : ys) =
      case go xs ys of
        Right out1 -> Right ((x1, y1) : out1)
        Left _ -> Left ()
    go _ _ = Left ()


instance NormalizeType (Scheme var) where
  normalize (Scheme' args1 body1) = Scheme' args1 (normalize body1)


instance TypeEq (Scheme TypeVariable) where
  typeEq (Scheme' args1 body1) (Scheme' args2 body2) =
    case pairSchemeVars args1 args2 of
      Right sub -> typeEq (replaceMonoTypeId sub body1) body2
      Left _ -> False


{- | Closes the variables of a scheme by generating new
fresh type variables for it.
-}
instanceScheme
  :: IdGenerator TypeVariableId :> es
  => Scheme InferenceVariable
  -> Eff es (MonoType InferenceVariable)
instanceScheme s = do
  newIdsMap <-
    mapM
      ( \arg -> do
          newId <- generateId Nothing
          pure (from arg, Variable $ RealTypeVariable newId)
      )
      (NonEmpty.toList s.arguments)
  let
    context = Map.fromList newIdsMap
  pure $ replaceMonoVars context s.body


hasTypeVarScheme
  :: InferenceVariable
  -> Scheme InferenceVariable
  -> Bool
hasTypeVarScheme (ErrorVariable _) _ = False
hasTypeVarScheme v@(RealTypeVariable vid) (Scheme' args body) =
  notElem vid args && hasTypeVarMono v body


instance From outVar inVar => From (Scheme outVar) (Scheme inVar) where
  from Scheme' {arguments, body} =
    Scheme'
      { arguments = arguments
      , body = from body
      }


data Type var
  = TMono (MonoType var)
  | TPoly (Scheme var)
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (Type var)


instance Pretty var => Pretty (Type var) where
  pretty (TMono v) = pretty v
  pretty (TPoly v) = pretty v


instance From (Type var) TypeValue where
  from = TMono <<< from


instance From (Type var) (MonoType var) where
  from = TMono


instance From (Type var) (Scheme var) where
  from = TPoly


instance From outVar inVar => From (Type outVar) (Type inVar) where
  from (TMono m) = TMono (from m)
  from (TPoly m) = TPoly (from m)


instance
  (Eq var, Ord var, From var TypeVariableId)
  => FreeVariables var (Type var)
  where
  freeVariables (TMono m) = freeVariables m
  freeVariables (TPoly s) = freeVariables s


instance NormalizeType (Type var) where
  normalize (TMono m) = TMono (normalize m)
  normalize (TPoly s) = TPoly (normalize s)


instance TypeEq (Type TypeVariable) where
  typeEq (TMono m) (TMono m2) = typeEq m m2
  typeEq (TPoly s) (TPoly s2) = typeEq s s2
  typeEq _ _ = False


{- | Closes the variables of a type (if it has) by generating new
fresh type variables for it.
-}
instanceType
  :: IdGenerator TypeVariableId :> es
  => Type InferenceVariable
  -> Eff es (MonoType InferenceVariable)
instanceType (TMono x) = pure x
instanceType (TPoly s) = instanceScheme s


hasTypeVar
  :: InferenceVariable
  -> Type InferenceVariable
  -> Bool
hasTypeVar v (TMono ty) = hasTypeVarMono v ty
hasTypeVar v (TPoly ty) = hasTypeVarScheme v ty
