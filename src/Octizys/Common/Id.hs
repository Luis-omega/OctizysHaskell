module Octizys.Common.Id
  ( Id
      ( Id'
      , idRaw
      )
  , ExpressionVariableId (qualifier, uniqueId)
  , TypeVariableId (qualifier, uniqueId)
  , InfoId (qualifier, uniqueId)
  , GenerateIdFromInt (generateIdFromInt)
  ) where

import Octizys.Common.Qualifier (Qualifier)

import Data.Aeson (ToJSON)
import GHC.Generics (Generic, Generically (..))


newtype Id = Id' {idRaw :: Int}
  deriving
    ( Eq
    , Ord
    )
    via Int
  deriving (Show, Generic)
  deriving (ToJSON) via Generically Id


class GenerateIdFromInt a where
  generateIdFromInt :: Qualifier -> Int -> a


data InfoId = InfoId'
  { qualifier :: Qualifier
  , uniqueId :: Id
  }
  deriving (Eq, Ord, Show, Generic)
  deriving (ToJSON) via Generically InfoId


instance GenerateIdFromInt InfoId where
  generateIdFromInt q x = InfoId' q (Id' x)


data TypeVariableId = TypeVariableId'
  { qualifier :: Qualifier
  , uniqueId :: Id
  }
  deriving (Eq, Ord, Show, Generic)
  deriving (ToJSON) via Generically TypeVariableId


instance GenerateIdFromInt TypeVariableId where
  generateIdFromInt q x = TypeVariableId' q (Id' x)


data ExpressionVariableId = ExpressionVariableId'
  { qualifier :: Qualifier
  , uniqueId :: Id
  }
  deriving (Eq, Ord, Show, Generic)
  deriving (ToJSON) via Generically ExpressionVariableId


instance GenerateIdFromInt ExpressionVariableId where
  generateIdFromInt q x = ExpressionVariableId' q (Id' x)
