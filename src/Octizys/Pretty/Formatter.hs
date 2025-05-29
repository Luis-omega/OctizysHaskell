{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Octizys.Pretty.Formatter (Formatter (format)) where

import Data.Text (Text)
import qualified Octizys.Ast.Expression as Ast
import qualified Octizys.Ast.Node as Ast
import Octizys.Ast.Type (InferenceVariable)
import qualified Octizys.Ast.Type as Ast
import qualified Octizys.Cst.Comment as Cst
import qualified Octizys.Cst.Expression as Cst
import qualified Octizys.Cst.Node as Cst
import qualified Octizys.Cst.TopItem as Cst
import qualified Octizys.Cst.Type as Cst
import qualified Octizys.Pretty.Ast.Expression as Ast.Expression
import qualified Octizys.Pretty.Ast.Type as Ast.Type
import qualified Octizys.Pretty.Cst.Comment as Cst.Expression
import qualified Octizys.Pretty.Cst.Expression as Cst.Expression
import qualified Octizys.Pretty.Cst.TopItem as Cst.TopItem
import qualified Octizys.Pretty.Cst.Type as Cst.Type
import Octizys.Pretty.FormatContext (FormatContext, formatExpressionVar)
import Prettyprinter (Doc, pretty)
import qualified Prettyprinter as Pretty


class Formatter ann env t | env -> ann where
  format :: env -> t -> Doc ann


instance Formatter ann (FormatContext ann) Text where
  format _ = pretty


instance
  ( Formatter ann (FormatContext ann) a
  , Formatter ann (FormatContext ann) b
  )
  => Formatter ann (FormatContext ann) (a, b)
  where
  format ctx (a, b) = Pretty.parens (format ctx a <> format ctx b)


instance
  Formatter ann (FormatContext ann) a
  => Formatter ann (FormatContext ann) (Maybe a)
  where
  format ctx = maybe mempty (format ctx)


instance
  Formatter ann (FormatContext ann) var
  => Formatter ann (FormatContext ann) (Ast.Type var)
  where
  format = Ast.Type.format format


instance
  Formatter ann (FormatContext ann) var
  => Formatter ann (FormatContext ann) (Ast.Value var)
  where
  format = Ast.Expression.formatValue format


instance Formatter ann (FormatContext ann) InferenceVariable where
  format = Ast.Type.formatInferenceVariable


instance Formatter ann (FormatContext ann) Ast.TypeVariable where
  format = Ast.Type.formatTypeVariable


instance
  Formatter ann (FormatContext ann) var
  => Formatter ann (FormatContext ann) (Ast.MonoType var)
  where
  format = Ast.Type.formatMono format


instance
  Formatter ann (FormatContext ann) var
  => Formatter ann (FormatContext ann) (Ast.Scheme var)
  where
  format = Ast.Type.formatScheme format


instance
  Formatter ann (FormatContext ann) var
  => Formatter ann (FormatContext ann) (Ast.Definition var)
  where
  format = Ast.Expression.formatDefinition format


instance
  Formatter ann (FormatContext ann) var
  => Formatter ann (FormatContext ann) (Ast.Expression var)
  where
  format = Ast.Expression.formatExpression format


instance
  Formatter ann (FormatContext ann) var
  => Formatter ann (FormatContext ann) (Ast.Node var)
  where
  format ctx (Ast.NType t) = format ctx t
  format ctx (Ast.NValue t) = format ctx t
  format ctx (Ast.NDef t) = format ctx t
  format ctx (Ast.NParameter (vid, ty)) =
    Pretty.parens
      ( formatExpressionVar ctx vid
          <> pretty ':'
          <> format ctx ty
      )
  format ctx (Ast.NExp t) = format ctx t


instance Formatter ann (FormatContext ann) Cst.Type where
  format = Cst.Type.format


instance Formatter ann (FormatContext ann) Cst.LineComment where
  format _ = Cst.Expression.formatLine


instance Formatter ann (FormatContext ann) Cst.BlockComment where
  format _ = Cst.Expression.formatBlock


instance Formatter ann (FormatContext ann) Cst.Comment where
  format _ = Cst.Expression.formatComment


instance Formatter ann (FormatContext ann) Cst.Parameter where
  format = Cst.Expression.formatParameter


instance Formatter ann (FormatContext ann) Cst.Parameters where
  format = Cst.Expression.formatParameters


instance Formatter ann (FormatContext ann) Cst.Definition where
  format = Cst.Expression.formatDefinition


instance Formatter ann (FormatContext ann) Cst.Function where
  format = Cst.Expression.formatFunction


instance Formatter ann (FormatContext ann) Cst.Expression where
  format = Cst.Expression.formatExpression


instance Formatter ann (FormatContext ann) Cst.Node where
  format ctx (Cst.NType t) = format ctx t
  format ctx (Cst.NParam t) = format ctx t
  format ctx (Cst.NDef t) = format ctx t
  format ctx (Cst.NFunction t) = format ctx t
  format ctx (Cst.NExp t) = format ctx t


instance Formatter ann (FormatContext ann) Cst.Module where
  format = Cst.TopItem.formatModule

