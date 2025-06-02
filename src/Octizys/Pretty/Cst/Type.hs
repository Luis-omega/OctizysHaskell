module Octizys.Pretty.Cst.Type (needsParentsInArrow, format) where

import Control.Arrow ((<<<))
import Data.List.NonEmpty (cons)
import Data.Text (Text)
import Octizys.Cst.Type
  ( Type
      ( Arrow
      , BoolType
      , IntType
      , Parens
      , TVariable
      , remain
      , start
      , variable
      )
  , _type
  )
import Octizys.Pretty.FormatContext
  ( FormatContext
  , nest
  )
import Prettyprinter (Doc, Pretty (pretty))
import qualified Prettyprinter as Pretty


needsParentsInArrow :: Type tvar -> Bool
needsParentsInArrow t =
  case t of
    IntType {} -> False
    BoolType {} -> False
    Arrow {} -> True
    Parens {} -> True
    TVariable {} -> False


format
  :: (FormatContext ann -> tvar -> Doc ann)
  -> FormatContext ann
  -> Type tvar
  -> Doc ann
format formatTVar ctx t =
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
            then Pretty.parens (format formatTVar ctx ty)
            else format formatTVar ctx ty
    Parens {_type = t2} ->
      format formatTVar ctx t2
    TVariable {variable = v} -> formatTVar ctx v

