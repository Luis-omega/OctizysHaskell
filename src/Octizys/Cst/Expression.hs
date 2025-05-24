module Octizys.Cst.Expression
  ( Parameter (ParameterAlone, ParameterWithType, name, _type, colon)
  , FunctionParameter
    ( FunctionParameterAlone
    , FunctionParameterWithType
    , lparen
    , rparen
    , parameter
    )
  , Definition
    ( Definition'
    , name
    , parameters
    , equal
    , definition
    , outputType
    , colon
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

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import Octizys.Cst.InfoId
  ( HasInfoSpan (getInfoSpan)
  , InfoId
  , InfoSpan (OneInfo, TwoInfo)
  , infoSpanEnd
  )
import Octizys.Cst.Type (Type)
import Octizys.Cst.VariableId (VariableId)
import Octizys.Effects.Generator.Interpreter (GenerateFromInt)
import Prettyprinter (Pretty (pretty))


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


instance Pretty ExpressionVariableId where
  pretty (ExpressionVariableId' ex) =
    pretty @Text "_e" <> pretty ex


-- | The set of parameters
data Parameter
  = ParameterAlone {name :: (InfoId, ExpressionVariableId)}
  | ParameterWithType
      { name :: (InfoId, ExpressionVariableId)
      , colon :: InfoId
      , _type :: Type
      }
  deriving (Show, Eq, Ord)


instance HasInfoSpan Parameter where
  getInfoSpan ParameterAlone {name = inf} = OneInfo (fst inf)
  getInfoSpan ParameterWithType {name = inf} = OneInfo (fst inf)


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


instance HasInfoSpan FunctionParameter where
  getInfoSpan FunctionParameterWithType {lparen, rparen} = TwoInfo lparen rparen
  getInfoSpan FunctionParameterAlone {parameter} = getInfoSpan parameter


newtype Parameters = Parameters' {unParameters :: NonEmpty (Parameter, InfoId)}
  deriving (Show, Eq, Ord)


-- | Either a Let definition or a Top level definition
data Definition = Definition'
  { name :: (InfoId, ExpressionVariableId)
  , colon :: Maybe InfoId
  , parameters :: Maybe Parameters
  , outputType :: Maybe Type
  , equal :: InfoId
  , definition :: Expression
  }
  deriving (Show, Eq, Ord)


instance HasInfoSpan Definition where
  getInfoSpan d =
    TwoInfo
      (fst d.name)
      ( infoSpanEnd $
          getInfoSpan d.definition
      )


-- | A lambda function.
data Function = Function'
  { start :: InfoId
  , -- This is the difference between `Definition` and `Function`
    -- The InfoIds represents parens
    parameters :: NonEmpty FunctionParameter
  , arrow :: InfoId
  , body :: Expression
  }
  deriving (Show, Eq, Ord)


instance HasInfoSpan Function where
  getInfoSpan f = TwoInfo f.start (infoSpanEnd $ getInfoSpan f.body)


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


instance HasInfoSpan Expression where
  getInfoSpan EInt {info} = OneInfo info
  getInfoSpan EBool {info} = OneInfo info
  getInfoSpan Variable {info} = OneInfo info
  getInfoSpan Parens {lparen, rparen} = TwoInfo lparen rparen
  getInfoSpan EFunction {functionValue} = getInfoSpan functionValue
  getInfoSpan Application {applicationFunction, applicationRemain} =
    getInfoSpan applicationFunction
      <> getInfoSpan (NonEmpty.last applicationRemain)
  getInfoSpan If {_if, ifFalse} =
    TwoInfo _if (infoSpanEnd $ getInfoSpan ifFalse)
  getInfoSpan Let {_let, expression} =
    TwoInfo _let (infoSpanEnd $ getInfoSpan expression)
  getInfoSpan Annotation {expression, _type} =
    getInfoSpan expression <> getInfoSpan _type
