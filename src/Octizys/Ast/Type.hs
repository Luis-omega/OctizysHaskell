{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Octizys.Ast.Type (Type (TMono, TPoly), instanceType, hasTypeVar) where

import Control.Arrow ((<<<))
import Data.Aeson (ToJSON)
import GHC.Generics (Generic, Generically (..))

import Effectful (Eff, (:>))

import Octizys.Ast.Type.Basics
  ( InferenceVariable
  , NormalizeType (normalize)
  , TypeEq (typeEq)
  , TypeValue
  , TypeVariable
  )
import Octizys.Ast.Type.MonoType (MonoType)
import qualified Octizys.Ast.Type.MonoType as Mono
import Octizys.Ast.Type.Scheme (Scheme, instanceScheme)
import qualified Octizys.Ast.Type.Scheme as Scheme
import Octizys.Classes.FreeVariables (FreeVariables (freeVariables))
import Octizys.Classes.From (From (from))
import Octizys.Common.Id (TypeVariableId)
import Octizys.Effects.IdGenerator.Effect (IdGenerator)
import Octizys.Format.Class (Formattable (format))


data Type var
  = TMono (MonoType var)
  | TPoly (Scheme var)
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (Type var)


instance Formattable var => Formattable (Type var) where
  format c (TMono v) = format c v
  format c (TPoly v) = format c v


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
hasTypeVar v (TMono ty) = Mono.hasTypeVar v ty
hasTypeVar v (TPoly ty) = Scheme.hasTypeVar v ty
