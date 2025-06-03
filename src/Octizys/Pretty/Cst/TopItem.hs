module Octizys.Pretty.Cst.TopItem where

import Octizys.Cst.TopItem (Module (Module', definitions, lastComments))
import Octizys.Pretty.Cst.Expression (formatDefinition)
import Octizys.Pretty.FormatContext (FormatContext)
import Prettyprinter (Doc, vsep)


formatModule
  :: ( FormatContext ann
       -> evar
       -> Doc ann
     )
  -> ( FormatContext ann
       -> tvar
       -> Doc ann
     )
  -> FormatContext ann
  -> Module evar tvar
  -> Doc ann
formatModule
  fmtEvar
  fmtTvar
  ctx
  ( Module'
      { lastComments = _lastComment
      , definitions = _definitions
      }
    ) =
    -- TODO: missing last comment!
    vsep
      ( formatDefinition
          fmtEvar
          fmtTvar
          ctx
          <$> (fst <$> _definitions)
      )
