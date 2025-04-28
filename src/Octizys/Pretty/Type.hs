module Octizys.Pretty.Type (needsParentsInArrow, prettyType) where

import Control.Arrow ((<<<))
import Data.List.NonEmpty (cons)
import Data.Text (Text)
import Octizys.Cst.Type
  ( Type (Arrow, BoolType, IntType, Parens, Variable, remain, start)
  , TypeVariableId
  , variableId
  , _type
  )
import Prettyprinter (Doc, Pretty (pretty))
import qualified Prettyprinter as Pretty


needsParentsInArrow :: Type -> Bool
needsParentsInArrow t =
  case t of
    IntType {} -> False
    BoolType {} -> False
    Arrow {} -> True
    Parens {} -> True
    Variable {} -> False


prettyType :: (TypeVariableId -> Doc ann) -> Type -> Doc ann
prettyType prettyVar t =
  case t of
    IntType _ -> pretty @Text "int"
    BoolType _ -> pretty @Text "bool"
    Arrow {start = _domain, remain = _codomain} ->
      (Pretty.group <<< Pretty.nest 2)
        ( Pretty.line'
            <> Pretty.concatWith
              (\l r -> l <> Pretty.line <> pretty @Text "->" <> r)
              ( prettyArg
                  <$> cons _domain _codomain
              )
        )
      where
        prettyArg ty =
          if needsParentsInArrow ty
            then Pretty.parens (prettyType prettyVar ty)
            else prettyType prettyVar ty
    Parens {_type = t2} ->
      prettyType prettyVar t2
    Variable {variableId = v} -> prettyVar v
