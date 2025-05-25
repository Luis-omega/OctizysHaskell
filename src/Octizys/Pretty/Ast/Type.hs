module Octizys.Pretty.Ast.Type (format, needsParentsInArrow) where

import Control.Arrow ((<<<))
import Data.List.NonEmpty (cons)
import Data.Text (Text)
import Octizys.Ast.Type
import Octizys.Pretty.FormatContext (FormatContext, formatTypeVar)
import qualified Octizys.Pretty.FormatContext as Format
import Prettyprinter (Doc, Pretty (pretty))
import qualified Prettyprinter as Pretty


needsParentsInArrow :: Type -> Bool
needsParentsInArrow t =
  case t of
    VType {} -> False
    Arrow {} -> True
    Variable {} -> False


formatValue :: FormatContext ann -> TypeValue -> Doc ann
formatValue _ BoolType = pretty @Text "Bool"
formatValue _ IntType = pretty @Text "Int"


format :: FormatContext ann -> Type -> Doc ann
format ctx VType {value} = formatValue ctx value
format ctx Arrow {start, remain} =
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
        then Pretty.parens (format ctx ty)
        else format ctx ty
format ctx Variable {variableId = v} = formatTypeVar ctx v
