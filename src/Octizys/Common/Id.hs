module Octizys.Common.Id
  ( Id
      ( Id'
      , idRaw
      )
  , SymbolContext (SymbolContext')
  , SymbolOriginInfo (qualifier, name)
  , Symbol
  , ExpressionVariableId
  , TypeVariableId
  , GenerateFromInt (generateFromInt)
  , HasSymbolStructure
    ( getPackageRef
    , getQualifier
    , getUniqueId
    , getOriginalName
    , getSymbol
    )
  ) where

import Octizys.Common.Qualifier (Qualifier)

import Control.Arrow ((<<<))
import Data.Aeson (ToJSON, ToJSONKey)
import qualified Data.List.NonEmpty as NonEmpty
import GHC.Generics (Generic, Generically (..))
import Octizys.Classes.From (From (from))
import Octizys.Common.LogicPath (LogicPath, logicPathSeparator)
import Octizys.Common.Name (Name)
import qualified Octizys.Package.Reference as Package
import Prettyprinter (Pretty, pretty)


newtype Id = Id' {idRaw :: Int}
  deriving
    ( Eq
    , Ord
    )
    via Int
  deriving (Show, Generic)
  deriving (Pretty) via Int
  deriving (ToJSON) via Generically Id


data SymbolContext = SymbolContext'
  { packageRef :: Package.Reference
  , qualifier :: Qualifier
  }
  deriving (Eq, Ord, Show, Generic)
  deriving (ToJSON) via Generically SymbolContext


instance Pretty SymbolContext where
  pretty s = pretty s.packageRef <> pretty s.qualifier


data SymbolOriginInfo = SymbolOriginInfo'
  { name :: Name
  , qualifier :: Maybe LogicPath
  }
  deriving (Eq, Ord, Show, Generic)
  deriving (ToJSON) via Generically SymbolOriginInfo


instance Pretty SymbolOriginInfo where
  pretty s =
    case s.qualifier of
      Just path -> pretty path <> pretty logicPathSeparator <> pretty s.name
      Nothing -> pretty s.name


instance From (Maybe LogicPath, Name) SymbolOriginInfo where
  from soi = (soi.qualifier, soi.name)


instance From SymbolOriginInfo ([Name], Name) where
  from (ps, name) =
    case ps of
      [] -> SymbolOriginInfo' {qualifier = Nothing, name}
      (p : remain) ->
        SymbolOriginInfo'
          { qualifier =
              Just (from (p NonEmpty.:| remain))
          , name
          }


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
  getPackageRef :: a -> Package.Reference
  getQualifier :: a -> Qualifier
  getUniqueId :: a -> Id
  getOriginalName :: a -> Maybe (Maybe LogicPath, Name)
  getSymbol :: a -> Symbol


instance GenerateFromInt Symbol where
  generateFromInt ctx info x = Symbol' ctx info (Id' x)


instance Pretty Symbol where
  pretty s =
    case s.originInfo of
      Just info -> pretty s.context.packageRef <> pretty logicPathSeparator <> pretty info
      Nothing ->
        pretty s.context
          <> pretty logicPathSeparator
          <> pretty 'v'
          <> pretty s.uniqueId


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


instance ToJSONKey TypeVariableId


instance GenerateFromInt TypeVariableId where
  generateFromInt ctx oinfo x =
    TypeVariableId' (generateFromInt ctx oinfo x)


instance Pretty TypeVariableId where
  pretty = pretty <<< getSymbol


newtype ExpressionVariableId = ExpressionVariableId'
  { getExpressionVariableIdSymbol :: Symbol
  }
  deriving (Eq, Ord, Show, HasSymbolStructure) via Symbol
  deriving (Generic)
  deriving (ToJSON) via Generically ExpressionVariableId


instance ToJSONKey ExpressionVariableId


instance GenerateFromInt ExpressionVariableId where
  generateFromInt ctx oinfo x =
    ExpressionVariableId' (generateFromInt ctx oinfo x)


instance Pretty ExpressionVariableId where
  pretty = pretty <<< getSymbol
