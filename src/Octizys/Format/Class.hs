module Octizys.Format.Class where

import Octizys.Format.Config (Configuration)
import Prettyprinter (Doc, Pretty (pretty))


class Formattable a where
  format :: Configuration -> a -> Doc ann


instance Formattable String where
  format _ = pretty
