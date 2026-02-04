module Octizys.Ast.Format.Type
  ( formatMono
  , needsParentsInArrow
  , formatValue
  , formatScheme
  , format
  ) where

import Control.Arrow ((<<<))
import Data.List.NonEmpty (cons)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import Octizys.Ast.Type
import Octizys.Common.Format.Config (nest)
import qualified Octizys.Common.Format.Config as Format
import Prettyprinter (Doc, Pretty (pretty))
import qualified Prettyprinter as Pretty


formatValue :: Format.Configuration -> TypeValue -> Doc ann
formatValue _ BoolType = pretty @Text "Bool"
formatValue _ IntType = pretty @Text "Int"


formatMono
  :: (Format.Configuration -> var -> Doc ann)
  -> Format.Configuration
  -> MonoType var
  -> Doc ann
formatMono _ configuration VType {value} = formatValue configuration value
formatMono fmtVar configuration Arrow {start, remain} =
  (Pretty.group <<< Format.nest configuration)
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
        then Pretty.parens (formatMono fmtVar configuration ty)
        else formatMono fmtVar configuration ty
formatMono fmtVar configuration (Variable v) = fmtVar configuration v


formatScheme
  :: (Format.Configuration -> var -> Doc ann)
  -> Format.Configuration
  -> Scheme var
  -> Doc ann
formatScheme fmtVar configuration (Scheme' {arguments, body}) =
  pretty @Text "forall"
    <> nest
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
    <> nest
      configuration
      ( formatMono fmtVar configuration body
      )


format
  :: (Format.Configuration -> var -> Doc ann)
  -> Format.Configuration
  -> Type var
  -> Doc ann
format fmtVar configuration (TMono t) = formatMono fmtVar configuration t
format fmtVar configuration (TPoly t) = formatScheme fmtVar configuration t
