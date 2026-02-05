{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Octizys.Ast.Type where

import Control.Arrow ((<<<))
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Set (singleton)
import qualified Data.Set as Set
import Octizys.Classes.FreeVariables (FreeVariables (freeVariables))
import Octizys.Classes.From (From (from))
import Octizys.FrontEnd.Cst.Type (TypeVariableId)

import Data.Aeson (ToJSON, ToJSONKey)
import Data.List.NonEmpty (cons)
import Data.Map (Map)
import Data.Text (Text)
import Effectful (Eff, (:>))
import GHC.Generics (Generic, Generically (..))
import Octizys.Common.Format (defaultIndentationSpaces)
import Octizys.Effects.IdGenerator.Effect (IdGenerator)
import Prettyprinter (Pretty, pretty, (<+>))
import qualified Prettyprinter as Pretty


data TypeValue = BoolType | IntType
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically TypeValue


instance FreeVariables TypeVariableId TypeValue where
  freeVariables _ = mempty


instance Pretty TypeValue where
  pretty BoolType = pretty @Text "Bool"
  pretty IntType = pretty @Text "Int"


data InferenceVariable
  = ErrorVariable String
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


newtype TypeVariable = TypeVariable' {unTypeVariable :: TypeVariableId}
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically TypeVariable


instance Pretty TypeVariable where
  pretty (TypeVariable' vid) = pretty vid


instance FreeVariables TypeVariableId TypeVariable where
  freeVariables v = singleton v.unTypeVariable


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
  FreeVariables TypeVariableId var
  => FreeVariables TypeVariableId (MonoType var)
  where
  freeVariables VType {value} = freeVariables value
  freeVariables Arrow {start, remain} =
    foldl'
      (<>)
      (freeVariables start)
      (freeVariables <$> remain)
  freeVariables (Variable v) = freeVariables v


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
  FreeVariables TypeVariableId var
  => FreeVariables TypeVariableId (Scheme var)
  where
  freeVariables s =
    Set.difference
      (freeVariables s.body)
      (Set.fromList (NonEmpty.toList s.arguments))


-- TODO:FIXME

{- | Closes the variables of a scheme by generating new
fresh type variables for it.
-}
instanceScheme
  :: IdGenerator TypeVariableId :> es
  => Scheme var
  -> Eff es (MonoType var)
instanceScheme _ = undefined


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
  FreeVariables TypeVariableId var
  => FreeVariables TypeVariableId (Type var)
  where
  freeVariables (TMono m) = freeVariables m
  freeVariables (TPoly s) = freeVariables s


{- | Closes the variables of a type (if it has) by generating new
fresh type variables for it.
-}
instanceType
  :: IdGenerator TypeVariableId :> es
  => Type var
  -> Eff es (MonoType var)
instanceType (TMono x) = pure x
instanceType (TPoly s) = instanceScheme s


hasTypeVar
  :: InferenceVariable
  -> Type InferenceVariable
  -> Bool
hasTypeVar v (TMono ty) = hasTypeVarMono v ty
hasTypeVar v (TPoly ty) = hasTypeVarScheme v ty
