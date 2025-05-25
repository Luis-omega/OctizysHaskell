module Octizys.Pretty.Cst.TopItem where

import Octizys.Cst.TopItem (Module (Module', definitions, lastComments))
import Octizys.Pretty.Cst.Expression (formatDefinition)
import Octizys.Pretty.FormatContext (FormatContext)
import Prettyprinter (Doc, vsep)


formatModule
  :: FormatContext ann
  -> Module
  -> Doc ann
formatModule
  ctx
  ( Module'
      { lastComments = _lastComment
      , definitions = _definitions
      }
    ) =
    -- TODO: missing last comment!
    vsep
      ( formatDefinition
          ctx
          <$> (fst <$> _definitions)
      )
