module Octizys.FrontEnd.Format.Type (needsParentsInArrow, format) where

import Control.Arrow ((<<<))
import Data.List.NonEmpty (cons)
import Data.Text (Text)
import Octizys.Common.Format.Config
  ( nest
  )
import qualified Octizys.Common.Format.Config as Format
import Octizys.FrontEnd.Cst.Type
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
  :: Pretty tvar
  => Format.Configuration
  -> Type tvar
  -> Doc ann
format configuration t =
  case t of
    IntType _ -> pretty @Text "Int"
    BoolType _ -> pretty @Text "Bool"
    Arrow {start = _domain, remain = _codomain} ->
      (Pretty.group <<< nest configuration)
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
            then Pretty.parens (format configuration ty)
            else format configuration ty
    Parens {_type = t2} ->
      format configuration t2
    TVariable {variable = v} -> pretty v
