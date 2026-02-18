module Octizys.Format.Config
  ( AstConfiguration
  , defaultAstConfiguration
  , CstConfiguration
  , defaultCstConfiguration
  , ConstraintsConfiguration
  , defaultConstraintsConfiguration
  , Configuration
  , defaultConfiguration
  , makeConfiguration
  , shouldAstShowTypes
  , shouldConstraintShowReason
  , getIndentation
  ) where


data AstConfiguration = AstConfiguration'
  { showTypes :: Bool
  }
  deriving (Show, Eq, Ord)


defaultAstConfiguration :: AstConfiguration
defaultAstConfiguration =
  AstConfiguration'
    { showTypes = True
    }


data CstConfiguration = CstConfiguration' {}
  deriving (Show, Eq, Ord)


defaultCstConfiguration :: CstConfiguration
defaultCstConfiguration =
  CstConfiguration'
    {
    }


makeCstConfiguration :: CstConfiguration
makeCstConfiguration = CstConfiguration'


data ConstraintsConfiguration = ConstraintsConfiguration'
  { showReason :: Bool
  , showId :: Bool
  , showParentId :: Bool
  , showDependents :: Bool
  }
  deriving (Show, Eq, Ord)


defaultConstraintsConfiguration :: ConstraintsConfiguration
defaultConstraintsConfiguration =
  ConstraintsConfiguration'
    { showReason = True
    , showId = True
    , showParentId = True
    , showDependents = True
    }


data Configuration = Configuration'
  { ast :: AstConfiguration
  , cst :: CstConfiguration
  , constraints :: ConstraintsConfiguration
  , indentation :: Int
  }
  deriving (Show, Eq, Ord)


defaultConfiguration :: Configuration
defaultConfiguration =
  Configuration'
    { ast = defaultAstConfiguration
    , cst = defaultCstConfiguration
    , constraints = defaultConstraintsConfiguration
    , indentation = 2
    }


makeConfiguration
  :: AstConfiguration
  -> CstConfiguration
  -> ConstraintsConfiguration
  -> Int
  -> Configuration
makeConfiguration = Configuration'


shouldAstShowTypes :: Configuration -> Bool
shouldAstShowTypes configuration = configuration.ast.showTypes


shouldConstraintShowReason :: Configuration -> Bool
shouldConstraintShowReason configuration = configuration.constraints.showReason


getIndentation :: Configuration -> Int
getIndentation c = c.indentation
