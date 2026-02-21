module Octizys.Ast.Type.Scheme
  ( Scheme (Scheme', arguments, body)
  , instanceScheme
  , hasTypeVar
  ) where

import Prettyprinter (Doc, pretty)
import qualified Prettyprinter as Pretty

import Data.Aeson (ToJSON)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Generics (Generic, Generically (..))

import Effectful (Eff, (:>))

import Octizys.Ast.Type.Basics
  ( InferenceVariable (ErrorVariable, RealTypeVariable)
  , NormalizeType (normalize)
  , TypeEq (typeEq)
  , TypeVariable
  )
import Octizys.Ast.Type.MonoType (MonoType)
import qualified Octizys.Ast.Type.MonoType as Mono
import Octizys.Classes.FreeVariables (FreeVariables (freeVariables))
import Octizys.Classes.From (From (from))
import Octizys.Common.Id (TypeVariableId)
import Octizys.Effects.IdGenerator.Effect (IdGenerator, generateId)
import Octizys.Format.Class (Formattable (format))
import qualified Octizys.Format.Config as Format
import qualified Octizys.Format.Utils as Format


data Scheme var = Scheme'
  { arguments :: NonEmpty TypeVariableId
  , body :: MonoType var
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically (Scheme var)


instance Formattable var => Formattable (Scheme var) where
  format = formatScheme


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
      Right sub -> typeEq (Mono.replaceTypeId sub body1) body2
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
          pure (from arg, Mono.MonoVariable $ RealTypeVariable newId)
      )
      (NonEmpty.toList s.arguments)
  let
    context = Map.fromList newIdsMap
  pure $ Mono.replaceVars context s.body


hasTypeVar
  :: InferenceVariable
  -> Scheme InferenceVariable
  -> Bool
hasTypeVar (ErrorVariable _) _ = False
hasTypeVar v@(RealTypeVariable vid) (Scheme' args body) =
  notElem vid args && Mono.hasTypeVar v body


instance From outVar inVar => From (Scheme outVar) (Scheme inVar) where
  from Scheme' {arguments, body} =
    Scheme'
      { arguments = arguments
      , body = from body
      }


formatScheme
  :: Formattable var
  => Format.Configuration
  -> Scheme var
  -> Doc ann
formatScheme configuration (Scheme' {arguments, body}) =
  pretty @Text "forall"
    <> Format.nest
      configuration
      ( Pretty.line
          <> Pretty.fillSep
            ( Pretty.punctuate
                (pretty ',')
                (pretty <$> NonEmpty.toList arguments)
            )
      )
    <> Pretty.line
    <> pretty '.'
    <> Format.nest
      configuration
      ( format configuration body
      )
