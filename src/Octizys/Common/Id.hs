module Octizys.Common.Id
  ( Id
      ( Id'
      , idRaw
      )
  , SymbolContext
  , SymbolOriginInfo
  , Symbol
  , ExpressionVariableId
  , TypeVariableId
  , GenerateFromInt (generateFromInt)
  , HasSymbolStructure
    ( getPackageRef
    , getQualifier
    , getUniqueId
    , getSymbol
    )
  ) where

import Octizys.Common.Qualifier (Qualifier)

import Data.Aeson (ToJSON)
import GHC.Generics (Generic, Generically (..))
import Octizys.Classes.From (From (from))
import Octizys.Common.LogicPath (LogicPath)
import Octizys.Common.Name (Name)
import Octizys.Common.PackageRef (PackageRef)


newtype Id = Id' {idRaw :: Int}
  deriving
    ( Eq
    , Ord
    )
    via Int
  deriving (Show, Generic)
  deriving (ToJSON) via Generically Id


data SymbolContext = SymbolContext'
  { packageRef :: PackageRef
  , qualifier :: Qualifier
  }
  deriving (Eq, Ord, Show, Generic)
  deriving (ToJSON) via Generically SymbolContext


data SymbolOriginInfo = SymbolOriginInfo'
  { name :: Name
  , qualifier :: Maybe LogicPath
  }
  deriving (Eq, Ord, Show, Generic)
  deriving (ToJSON) via Generically SymbolOriginInfo


instance From (Maybe LogicPath, Name) SymbolOriginInfo where
  from soi = (soi.qualifier, soi.name)


class GenerateFromInt a where
  generateFromInt
    :: SymbolContext
    -> Maybe
        SymbolOriginInfo
    -> Int
    -> a


data Symbol = Symbol'
  { context :: SymbolContext
  , originInfo :: Maybe SymbolOriginInfo
  , uniqueId :: Id
  }
  deriving (Eq, Ord, Show, Generic)
  deriving (ToJSON) via Generically Symbol


class HasSymbolStructure a where
  getPackageRef :: a -> PackageRef
  getQualifier :: a -> Qualifier
  getUniqueId :: a -> Id
  getOriginalName :: a -> Maybe (Maybe LogicPath, Name)
  getSymbol :: a -> Symbol


instance GenerateFromInt Symbol where
  generateFromInt ctx info x = Symbol' ctx info (Id' x)


instance HasSymbolStructure Symbol where
  getPackageRef s = s.context.packageRef
  getQualifier s = s.context.qualifier
  getUniqueId = uniqueId
  getOriginalName s = from <$> s.originInfo
  getSymbol x = x


newtype TypeVariableId = TypeVariableId'
  { getTypeVariableIdSymbol :: Symbol
  }
  deriving (Eq, Ord, Show, HasSymbolStructure) via Symbol
  deriving (Generic)
  deriving (ToJSON) via Generically TypeVariableId


instance GenerateFromInt TypeVariableId where
  generateFromInt ctx oinfo x =
    TypeVariableId' (generateFromInt ctx oinfo x)


newtype ExpressionVariableId = ExpressionVariableId'
  { getExpressionVariableIdSymbol :: Symbol
  }
  deriving (Eq, Ord, Show, HasSymbolStructure) via Symbol
  deriving (Generic)
  deriving (ToJSON) via Generically ExpressionVariableId


instance GenerateFromInt ExpressionVariableId where
  generateFromInt ctx oinfo x =
    ExpressionVariableId' (generateFromInt ctx oinfo x)
