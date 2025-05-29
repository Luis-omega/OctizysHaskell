module Octizys.Pretty.Cst.Type (needsParentsInArrow, format) where

import Control.Arrow ((<<<))
import Data.List.NonEmpty (cons)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import Octizys.Cst.Type
  ( Type
      ( Arrow
      , BoolType
      , IntType
      , Parens
      , Variable
      , remain
      , start
      )
  , variableId
  , _type
  )
import Octizys.Pretty.FormatContext
  ( FormatContext
  , formatTypeVar
  , nest
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


format :: FormatContext ann -> Type -> Doc ann
format ctx t =
  case t of
    IntType _ -> pretty @Text "Int"
    BoolType _ -> pretty @Text "Bool"
    Arrow {start = _domain, remain = _codomain} ->
      (Pretty.group <<< nest ctx)
        ( Pretty.line'
            <> Pretty.concatWith
              (\l r -> l <> Pretty.line <> pretty @Text "->" <> r)
              ( prettyArg
                  <$> cons _domain (snd <$> _codomain)
              )
        )
      where
        prettyArg ty =
          if needsParentsInArrow ty
            then Pretty.parens (format ctx ty)
            else format ctx ty
    Parens {_type = t2} ->
      format ctx t2
    Variable {variableId = v} -> formatTypeVar ctx v
