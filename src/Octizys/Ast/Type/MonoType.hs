{-# LANGUAGE DeriveFoldable #-}

module Octizys.Ast.Type.MonoType
  ( Arrow (Arrow', start, remain)
  , arrowFromArgsAndOutput
  , MonoType (MonoValue, MonoArrow, MonoVariable)
  , hasTypeVar
  , replaceVars
  , replaceTypeId
  , needsParentsInArrow
  ) where

import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import Octizys.Classes.FreeVariables (FreeVariables (freeVariables))
import Octizys.Classes.From (From (from))
import Octizys.FrontEnd.Cst.Type (TypeVariableId)

import Control.Arrow ((<<<))
import Data.Aeson (ToJSON)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe
import Data.Text (Text)
import GHC.Generics (Generic, Generically (..))
import Octizys.Ast.Type.Basics
  ( InferenceVariable
  , NormalizeType (normalize)
  , TypeEq (typeEq)
  , TypeValue
  , TypeVariable (TypeVariable')
  )
import Octizys.Format.Class (Formattable (format))
import qualified Octizys.Format.Config as Format
import qualified Octizys.Format.Utils as Format
import Prettyprinter (Doc, Pretty (pretty))
import qualified Prettyprinter as Pretty


-- * Arrow


data Arrow var = Arrow'
  { start :: MonoType var
  , remain :: NonEmpty (MonoType var)
  }
  deriving (Show, Eq, Ord, Generic, Functor, Foldable)
  deriving (ToJSON) via Generically (Arrow var)


instance Formattable var => Formattable (Arrow var) where
  format = formatArrow


instance TypeEq var => TypeEq (Arrow var) where
  typeEq (Arrow' start1 remain1) (Arrow' start2 remain2) =
    typeEq start1 start2 && typeEq remain1 remain2


instance NormalizeType (Arrow var) where
  normalize (Arrow' start1 remain1) =
    let
      start2 = normalize start1
      remain2 = normalize <$> remain1
     in
      flattenArrows start2 remain2
    where
      flattenArrows :: MonoType var -> NonEmpty (MonoType var) -> Arrow var
      flattenArrows initType original@(x :| []) =
        case x of
          MonoArrow (Arrow' x1 xs) -> Arrow' initType (NonEmpty.cons x1 xs)
          _ -> Arrow' initType original
      flattenArrows initType (x :| (x2 : remain)) =
        case flattenArrows x (x2 :| remain) of
          Arrow' newDom newRange ->
            Arrow' initType (NonEmpty.cons newDom newRange)


instance (Eq var, Ord var) => FreeVariables var (Arrow var) where
  freeVariables = foldMap Set.singleton


-- * Mono types


data MonoType var
  = MonoValue TypeValue
  | MonoArrow (Arrow var)
  | MonoVariable var
  deriving (Show, Eq, Ord, Generic, Functor, Foldable)
  deriving (ToJSON) via Generically (MonoType var)


instance From (MonoType var) TypeValue where
  from = MonoValue


instance From outVar inVar => From (MonoType outVar) (MonoType inVar) where
  from x = from <$> x


instance From (MonoType var) (Arrow var) where
  from = MonoArrow


instance Formattable var => Formattable (MonoType var) where
  format = formatMono


instance
  (Eq var, Ord var)
  => FreeVariables var (MonoType var)
  where
  freeVariables = foldMap Set.singleton


instance NormalizeType (MonoType var) where
  normalize (MonoArrow arr) = MonoArrow (normalize arr)
  normalize x = x


instance TypeEq var => TypeEq (MonoType var) where
  typeEq x y =
    case (normalize x, normalize y) of
      (MonoValue v1, MonoValue v2) -> typeEq v1 v2
      (MonoArrow a1, MonoArrow a2) -> typeEq a1 a2
      (MonoVariable v1, MonoVariable v2) -> typeEq v1 v2
      (_, _) -> False


{- | Creates and arrow whose arguments are all in a  list and
the output is separated.
-}
arrowFromArgsAndOutput
  :: NonEmpty (MonoType var)
  -> MonoType var
  -> Arrow var
arrowFromArgsAndOutput (start :| otherArgs) out =
  case otherArgs of
    [] -> Arrow' start (NonEmpty.singleton out)
    (x : xs) -> Arrow' start ((x :| xs) <> NonEmpty.singleton out)


-- * Predicates


-- | Verify if the given variable is inside the type.
hasTypeVar
  :: Eq var
  => var
  -> MonoType var
  -> Bool
hasTypeVar v = foldl' (\acc var -> (v == var) || acc) False


-- * Replacement of variables


replaceVars
  :: Map InferenceVariable (MonoType InferenceVariable)
  -> MonoType InferenceVariable
  -> MonoType InferenceVariable
replaceVars _ t@(MonoValue _) = t
replaceVars s (MonoArrow (Arrow' t1 t2)) =
  MonoArrow $
    Arrow'
      (replaceVars s t1)
      (replaceVars s <$> t2)
replaceVars s t@(MonoVariable v) =
  Data.Maybe.fromMaybe t (Map.lookup v s)


replaceTypeId
  :: Map TypeVariableId TypeVariableId
  -> MonoType TypeVariable
  -> MonoType TypeVariable
replaceTypeId _ t@(MonoValue _) = t
replaceTypeId s (MonoArrow (Arrow' t1 t2)) =
  MonoArrow $
    Arrow'
      (replaceTypeId s t1)
      (replaceTypeId s <$> t2)
replaceTypeId s t@(MonoVariable v) =
  case Map.lookup (from v) s of
    Just newId -> MonoVariable (TypeVariable' newId)
    Nothing -> t


-- * Format


needsParentsInArrow :: MonoType var -> Bool
needsParentsInArrow t =
  case t of
    MonoValue _ -> False
    MonoArrow _ -> True
    MonoVariable _ -> False


formatArrow
  :: Formattable var
  => Format.Configuration
  -> Arrow var
  -> Doc ann
formatArrow configuration Arrow' {start, remain} =
  (Pretty.group <<< Format.nest configuration)
    ( Pretty.line'
        <> Pretty.concatWith
          (\l r -> l <> Pretty.line <> pretty @Text "->" <> r)
          ( prettyArg
              <$> NonEmpty.cons start remain
          )
    )
  where
    prettyArg ty =
      if needsParentsInArrow ty
        then Pretty.parens (format configuration ty)
        else format configuration ty


formatMono
  :: Formattable var
  => Format.Configuration
  -> MonoType var
  -> Doc ann
formatMono configuration (MonoValue value) = format configuration value
formatMono configuration (MonoArrow arr) = format configuration arr
formatMono configuration (MonoVariable v) = format configuration v
