module Octizys.Common.Format.Config
  ( Configuration
  , defaultConfiguration
  , makeConfiguration
  , nest
  , formatText
  , shouldShowTypes
  , shouldShowConstraintReason
  ) where

import Data.Text (Text)
import Prettyprinter (Doc, Pretty (pretty))
import qualified Prettyprinter as Pretty


data Configuration = Configuration'
  { showTypeVars :: Bool
  , indentation :: Int
  , -- If we display the reason for a constraint
    showConstraintsReasons :: Bool
  }
  deriving (Show, Eq, Ord)


defaultConfiguration :: Configuration
defaultConfiguration =
  Configuration'
    { showTypeVars = False
    , indentation = 2
    , showConstraintsReasons = False
    }


makeConfiguration :: Bool -> Int -> Bool -> Configuration
makeConfiguration = Configuration'


nest :: Configuration -> Doc ann -> Doc ann
nest configuration = Pretty.nest configuration.indentation


formatText :: Text -> Doc ann
formatText = pretty


shouldShowTypes :: Configuration -> Bool
shouldShowTypes configuration = configuration.showTypeVars


shouldShowConstraintReason :: Configuration -> Bool
shouldShowConstraintReason configuration = configuration.showConstraintsReasons
