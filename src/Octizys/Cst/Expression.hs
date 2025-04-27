module Octizys.Cst.Expression
  ( Parameter (Parameter', name, _type)
  , Definition (Definition', name, parameters, equal, definition)
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
    , _type
    )
  , ExpressionVariableId (ExpressionVariableId')
  , Parameters (Parameters', start, remain)
  , freshExpressionVariableId
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Effectful (Eff, (:>))
import Octizys.Cst.InfoId (InfoId)
import Octizys.Cst.Type (Type)
import Octizys.Cst.VariableId (VariableId)
import Octizys.Effects.Generator (GenerateFromInt, Generator, generate)


{- | A Wrapper around VariableId to represent expressions
of variables.
-}
newtype ExpressionVariableId
  = ExpressionVariableId' VariableId
  deriving
    ( Show
    , Eq
    , Ord
    , GenerateFromInt
    )
    via VariableId


freshExpressionVariableId
  :: Generator ExpressionVariableId :> es
  => Eff es ExpressionVariableId
freshExpressionVariableId = generate


-- | The set of parameters
data Parameter = Parameter'
  { name :: (InfoId, ExpressionVariableId)
  , _type :: Maybe (InfoId, Type)
  -- ^ The alone info is the colon, we don't put as independent field
  -- as it appear only if the type is part of the parameter.
  }
  deriving (Show, Eq, Ord)


data Parameters = Parameters'
  { -- The a in "a->b"
    start :: Parameter
  , remain :: [(InfoId, Parameter)]
  -- ^ The  " -> b " in "a -> b"
  }
  deriving (Show, Eq, Ord)


-- | Either a Let definition or a Top level definition
data Definition = Definition'
  { name :: (InfoId, ExpressionVariableId)
  , parameters :: Maybe Parameters
  , equal :: InfoId
  , definition :: Expression
  }
  deriving (Show, Eq, Ord)


-- | A lambda function.
data Function = Function'
  { start :: InfoId
  , parameters :: NonEmpty Parameter
  , arrow :: InfoId
  , body :: Expression
  }
  deriving (Show, Eq, Ord)


data Expression
  = EInt {info :: InfoId, intValue :: Text}
  | EBool {info :: InfoId, boolValue :: Bool}
  | Variable {info :: InfoId, name :: ExpressionVariableId}
  | Parens
      { info :: InfoId
      , lparen :: InfoId
      , expression :: Expression
      , rparen :: InfoId
      }
  | EFunction
      { info :: InfoId
      , functionValue :: Function
      }
  | Application
      { info :: InfoId
      , applicationFunction :: Expression
      , applicationRemain :: NonEmpty Expression
      }
  | If
      { info :: InfoId
      , _if :: InfoId
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
      { info :: InfoId
      , expression :: Expression
      , colon :: InfoId
      , _type :: Type
      }
  deriving (Show, Eq, Ord)
