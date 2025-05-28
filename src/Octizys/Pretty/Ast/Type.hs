module Octizys.Pretty.Ast.Type
  ( formatMono
  , needsParentsInArrow
  , formatInferenceVariable
  , formatTypeVariable
  , formatValue
  , formatScheme
  , format
  ) where

import Control.Arrow ((<<<))
import Data.List.NonEmpty (cons)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import Octizys.Ast.Type
import Octizys.Pretty.FormatContext (FormatContext, formatTypeVar, nest)
import qualified Octizys.Pretty.FormatContext as Format
import Prettyprinter (Doc, Pretty (pretty))
import qualified Prettyprinter as Pretty


formatInferenceVariable :: FormatContext ann -> InferenceVariable -> Doc ann
formatInferenceVariable _ ErrorVariable = pretty @Text "ErrorVariable"
formatInferenceVariable ctx (MetaVariable vid) = formatTypeVar ctx vid


formatTypeVariable :: FormatContext ann -> TypeVariable -> Doc ann
formatTypeVariable ctx (TypeVariable tvid) = formatTypeVar ctx tvid


needsParentsInArrow :: MonoType var -> Bool
needsParentsInArrow t =
  case t of
    VType {} -> False
    Arrow {} -> True
    Variable {} -> False


formatValue :: FormatContext ann -> TypeValue -> Doc ann
formatValue _ BoolType = pretty @Text "Bool"
formatValue _ IntType = pretty @Text "Int"


formatMono
  :: (FormatContext ann -> var -> Doc ann)
  -> FormatContext ann
  -> MonoType var
  -> Doc ann
formatMono _ ctx VType {value} = formatValue ctx value
formatMono fmtVar ctx Arrow {start, remain} =
  (Pretty.group <<< Format.nest ctx)
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
        then Pretty.parens (formatMono fmtVar ctx ty)
        else formatMono fmtVar ctx ty
formatMono fmtVar ctx (Variable v) = fmtVar ctx v


formatScheme
  :: (FormatContext ann -> var -> Doc ann)
  -> FormatContext ann
  -> Scheme var
  -> Doc ann
formatScheme fmtVar ctx (Scheme' {arguments, body}) =
  pretty @Text "forall"
    <> nest
      ctx
      ( Pretty.line
          <> Pretty.fillSep
            ( Pretty.punctuate
                (pretty ',')
                (formatTypeVar ctx <$> NonEmpty.toList arguments)
            )
      )
    <> Pretty.line
    <> pretty '.'
    <> nest
      ctx
      ( formatMono fmtVar ctx body
      )


format
  :: (FormatContext ann -> var -> Doc ann)
  -> FormatContext ann
  -> Type var
  -> Doc ann
format fmtVar ctx (TMono t) = formatMono fmtVar ctx t
format fmtVar ctx (TPoly t) = formatScheme fmtVar ctx t
