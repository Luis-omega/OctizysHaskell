module Octizys.Pretty.FormatContext
  ( Formatters
  , defaultFormatters
  , makeFormattersWithMap
  , Configuration
  , defaultConfiguration
  , makeConfiguration
  , FormatContext (configuration, formatters)
  , makeFormatContext
  , defaultFormatContext
  , formatTypeVar
  , formatExpressionVar
  , nest
  , formatText
  , shouldShowTypes
  , shouldShowConstraintReason
  , setShowTypeVar
  ) where

import Control.Arrow ((<<<))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Octizys.Cst.Expression
  ( ExpressionVariableId (unExpressionVariableId)
  )
import Octizys.Cst.Type (TypeVariableId (unTypeVariableId))
import Prettyprinter (Doc, Pretty (pretty))
import qualified Prettyprinter as Pretty


data Formatters ann = Formatters'
  { formatExpVar :: ExpressionVariableId -> Doc ann
  , formatTyVar :: TypeVariableId -> Doc ann
  }


defaultTyVariableFormatter :: TypeVariableId -> Doc ann
defaultTyVariableFormatter x =
  pretty @Text "_t"
    <> pretty ((show <<< unTypeVariableId) x)


defaultExpVariableFormatter :: ExpressionVariableId -> Doc ann
defaultExpVariableFormatter x =
  pretty @Text "_e"
    <> pretty ((show <<< unExpressionVariableId) x)


defaultFormatters :: Formatters ann
defaultFormatters =
  Formatters'
    { formatExpVar = defaultExpVariableFormatter
    , formatTyVar = defaultTyVariableFormatter
    }


makeFormattersWithMap
  :: Show a
  => Show b
  => Ord a
  => Ord b
  => (a -> Doc ann)
  -> (b -> Doc ann)
  -> Map ExpressionVariableId a
  -> Map TypeVariableId b
  -> Formatters ann
makeFormattersWithMap fa fb me mt =
  Formatters'
    { formatExpVar =
        makeFunction
          defaultExpVariableFormatter
          fa
          me
    , formatTyVar =
        makeFunction
          defaultTyVariableFormatter
          fb
          mt
    }
  where
    makeFunction
      :: forall c d ann
       . Show d
      => Ord d
      => (d -> Doc ann)
      -> (c -> Doc ann)
      -> Map d c
      -> (d -> Doc ann)
    makeFunction fallback f m x =
      maybe
        (fallback x)
        f
        (Map.lookup x m)


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


setShowTypeVar :: Bool -> FormatContext ann -> FormatContext ann
setShowTypeVar newVal f =
  f
    { configuration =
        f.configuration {showTypeVars = newVal}
    }


data FormatContext ann = FormatContext'
  { configuration :: Configuration
  , formatters :: Formatters ann
  }


makeFormatContext :: Configuration -> Formatters ann -> FormatContext ann
makeFormatContext = FormatContext'


defaultFormatContext :: FormatContext ann
defaultFormatContext =
  makeFormatContext defaultConfiguration defaultFormatters


formatTypeVar :: FormatContext ann -> TypeVariableId -> Doc ann
formatTypeVar ctx =
  ctx.formatters.formatTyVar


formatExpressionVar :: FormatContext ann -> ExpressionVariableId -> Doc ann
formatExpressionVar ctx =
  ctx.formatters.formatExpVar


nest :: FormatContext ann -> Doc ann -> Doc ann
nest ctx = Pretty.nest ctx.configuration.indentation


formatText :: Text -> Doc ann
formatText = pretty


shouldShowTypes :: FormatContext ann -> Bool
shouldShowTypes ctx = ctx.configuration.showTypeVars


shouldShowConstraintReason :: FormatContext ann -> Bool
shouldShowConstraintReason ctx = ctx.configuration.showConstraintsReasons
