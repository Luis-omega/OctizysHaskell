{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Octizys.Pretty.Formatter (Formatter (format)) where

import qualified Octizys.Ast.Expression as Ast
import qualified Octizys.Ast.Node as Ast
import qualified Octizys.Ast.Type as Ast
import qualified Octizys.Cst.Comment as Cst
import qualified Octizys.Cst.Expression as Cst
import qualified Octizys.Cst.Node as Cst
import qualified Octizys.Cst.Type as Cst
import qualified Octizys.Pretty.Ast.Expression as Ast.Expression
import qualified Octizys.Pretty.Ast.Type as Ast.Type
import qualified Octizys.Pretty.Cst.Comment as Cst.Expression
import qualified Octizys.Pretty.Cst.Expression as Cst.Expression
import qualified Octizys.Pretty.Cst.Type as Cst.Type
import Octizys.Pretty.FormatContext (FormatContext, formatExpressionVar)
import Prettyprinter (Doc, pretty)
import qualified Prettyprinter as Pretty


class Formatter ann env t | env -> ann where
  format :: env -> t -> Doc ann


instance Formatter ann (FormatContext ann) Ast.Type where
  format = Ast.Type.format


instance Formatter ann (FormatContext ann) Ast.Value where
  format = Ast.Expression.formatValue


instance Formatter ann (FormatContext ann) Ast.Definition where
  format = Ast.Expression.formatDefinition


instance Formatter ann (FormatContext ann) Ast.Expression where
  format = Ast.Expression.formatExpression


instance Formatter ann (FormatContext ann) Ast.Node where
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


instance Formatter ann (FormatContext ann) Cst.FunctionParameter where
  format = Cst.Expression.formatParameterFunction


instance Formatter ann (FormatContext ann) Cst.Definition where
  format = Cst.Expression.formatDefinition


instance Formatter ann (FormatContext ann) Cst.Function where
  format = Cst.Expression.formatFunction


instance Formatter ann (FormatContext ann) Cst.Expression where
  format = Cst.Expression.formatExpression


instance Formatter ann (FormatContext ann) Cst.Node where
  format ctx (Cst.NType t) = format ctx t
  format ctx (Cst.NParam t) = format ctx t
  format ctx (Cst.NFunParam t) = format ctx t
  format ctx (Cst.NDef t) = format ctx t
  format ctx (Cst.NFunction t) = format ctx t
  format ctx (Cst.NExp t) = format ctx t
