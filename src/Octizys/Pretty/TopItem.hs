module Octizys.Pretty.TopItem where

import Octizys.Cst.Expression (ExpressionVariableId)
import Octizys.Cst.TopItem (Module (Module', definitions, lastComments))
import Octizys.Cst.Type (TypeVariableId)
import Octizys.Pretty.Expression (prettyDefinition)
import Prettyprinter (Doc, vsep)


prettyModule
  :: (ExpressionVariableId -> Doc ann)
  -> (TypeVariableId -> Doc ann)
  -> Module
  -> Doc ann
prettyModule
  prettyExp
  prettyType
  ( Module'
      { lastComments = _lastComment
      , definitions = _definitions
      }
    ) =
    -- TODO: missing last comment!
    vsep
      ( prettyDefinition
          prettyExp
          prettyType
          <$> (fst <$> _definitions)
      )
