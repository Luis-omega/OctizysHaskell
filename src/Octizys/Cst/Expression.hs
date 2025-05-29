{-# HLINT ignore "Use guards" #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Octizys.Cst.Expression
  ( Parameter (ParameterAlone, ParameterWithType, name, _type, colon)
  , FunctionParameter
    ( FunctionParameterAlone
    , FunctionParameterWithType
    , lparen
    , rparen
    , parameter
    )
  , SchemeStart (SchemeStart', _forall, typeArguments, dot)
  , DefinitionTypeAnnotation
    ( DefinitionTypeAnnotation'
    , colon
    , schemeStart
    , parameters
    , outputType
    )
  , Definition
    ( Definition'
    , name
    , equal
    , definition
    , _type
    )
  , Function (Function', start, arrow, body, parameters)
  , Expression
    ( EInt
    , EBool
    , Variable
    , Parens
    , EFunction
    , Application
    , If
    , Let
    , Annotation
    , info
    , intValue
    , boolValue
    , name
    , lparen
    , rparen
    , functionValue
    , applicationFunction
    , applicationRemain
    , _if
    , condition
    , _then
    , ifTrue
    , _else
    , ifFalse
    , _let
    , definitions
    , _in
    , expression
    , colon
    , _type
    )
  , ExpressionVariableId
    ( ExpressionVariableId'
    , unExpressionVariableId
    )
  , Parameters (Parameters', unParameters)
  )
where

import Control.Arrow ((<<<))
import Data.Foldable (Foldable (foldl'))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Set as Set
import Data.Text (Text)
import Octizys.Classes.FreeVariables (FreeVariables (freeVariables))
import Octizys.Cst.InfoId
  ( InfoId
  )
import Octizys.Cst.Type (Type, TypeVariableId)
import Octizys.Cst.VariableId (VariableId)
import Octizys.Effects.Generator.Interpreter (GenerateFromInt)


{- | A Wrapper around VariableId to represent expressions
of variables.
-}
newtype ExpressionVariableId = ExpressionVariableId' {unExpressionVariableId :: VariableId}
  deriving
    ( Eq
    , Ord
    , GenerateFromInt
    )
    via VariableId
  deriving (Show)


instance FreeVariables ExpressionVariableId ExpressionVariableId where
  freeVariables = Set.singleton


-- | The set of parameters
data Parameter
  = ParameterAlone {name :: (InfoId, ExpressionVariableId)}
  | ParameterWithType
      { name :: (InfoId, ExpressionVariableId)
      , colon :: InfoId
      , _type :: Type
      }
  deriving (Show, Eq, Ord)


instance FreeVariables ExpressionVariableId Parameter where
  freeVariables ParameterAlone {name} = Set.singleton (snd name)
  freeVariables ParameterWithType {name} = Set.singleton (snd name)


data FunctionParameter
  = FunctionParameterWithType
      { lparen :: InfoId
      , parameter :: Parameter
      , rparen :: InfoId
      }
  | FunctionParameterAlone
      { parameter :: Parameter
      -- ^ This must be only  `ParameterWithType`
      }
  deriving (Show, Eq, Ord)


instance FreeVariables ExpressionVariableId FunctionParameter where
  freeVariables FunctionParameterWithType {parameter} = freeVariables parameter
  freeVariables FunctionParameterAlone {parameter} = freeVariables parameter


newtype Parameters = Parameters' {unParameters :: NonEmpty (Parameter, InfoId)}
  deriving (Show, Eq, Ord)


instance FreeVariables ExpressionVariableId Parameters where
  freeVariables p =
    foldl'
      (<>)
      mempty
      ( (freeVariables <<< fst) <$> p.unParameters
      )


-- | forall a b c .
data SchemeStart = SchemeStart'
  { _forall :: InfoId
  , typeArguments :: NonEmpty (InfoId, TypeVariableId)
  , dot :: InfoId
  }
  deriving (Show, Eq, Ord)


data DefinitionTypeAnnotation = DefinitionTypeAnnotation'
  { colon :: InfoId
  , schemeStart :: Maybe SchemeStart
  , -- x : a , FinalType
    parameters :: Maybe Parameters
  , outputType :: Type
  }
  deriving (Show, Eq, Ord)


-- | Either a Let definition or a Top level definition
data Definition = Definition'
  { name :: (InfoId, ExpressionVariableId)
  , _type :: Maybe DefinitionTypeAnnotation
  , equal :: InfoId
  , definition :: Expression
  }
  deriving (Show, Eq, Ord)


-- | A lambda function.
data Function = Function'
  { start :: InfoId
  , parameters :: NonEmpty FunctionParameter
  , arrow :: InfoId
  , body :: Expression
  }
  deriving (Show, Eq, Ord)


data Expression
  = EInt {info :: InfoId, intValue :: Text}
  | EBool {info :: InfoId, boolValue :: Bool}
  | Variable {info :: InfoId, name :: ExpressionVariableId}
  | Parens
      { lparen :: InfoId
      , expression :: Expression
      , rparen :: InfoId
      }
  | EFunction
      { functionValue :: Function
      }
  | Application
      { applicationFunction :: Expression
      , applicationRemain :: NonEmpty Expression
      }
  | If
      { _if :: InfoId
      , condition :: Expression
      , _then :: InfoId
      , ifTrue :: Expression
      , _else :: InfoId
      , ifFalse :: Expression
      }
  | Let
      { _let :: InfoId
      , -- The alone info is the semicolon finishing a definition
        definitions :: NonEmpty (Definition, InfoId)
      , _in :: InfoId
      , expression :: Expression
      }
  | Annotation
      { expression :: Expression
      , colon :: InfoId
      , _type :: Type
      }
  deriving (Show, Eq, Ord)
