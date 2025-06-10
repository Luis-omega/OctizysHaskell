{-# HLINT ignore "Use guards" #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Octizys.Cst.Expression
  ( Parameter (ParameterAlone, ParameterWithType, name, _type, colon)
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
    , start
    , body
    , parameters
    )
  , Parameters
    ( Parameters'
    , initParameter
    , otherParameters
    , bodySeparator
    )
  )
where

import Control.Arrow ((<<<))
import Data.Foldable (Foldable (foldl'))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Set as Set
import Data.Text (Text)
import Octizys.Classes.FreeVariables (FreeVariables (freeVariables))
import Octizys.Cst.SourceInfo (SourceInfo)
import Octizys.Cst.Type (Type)


-- | The set of parameters
data Parameter evar tvar
  = ParameterAlone {name :: (SourceInfo, evar)}
  | ParameterWithType
      { name :: (SourceInfo, evar)
      , colon :: SourceInfo
      , _type :: Type tvar
      }
  deriving (Show, Eq, Ord)


instance Ord evar => FreeVariables evar (Parameter evar tvar) where
  freeVariables ParameterAlone {name} = Set.singleton (snd name)
  freeVariables ParameterWithType {name} = Set.singleton (snd name)


data Parameters evar tvar = Parameters'
  { initParameter :: Parameter evar tvar
  , otherParameters :: [(SourceInfo, Parameter evar tvar)]
  , bodySeparator :: SourceInfo
  }
  deriving (Show, Eq, Ord)


instance Ord evar => FreeVariables evar (Parameters evar tvar) where
  freeVariables p =
    foldl'
      (<>)
      (freeVariables p.initParameter)
      ( (freeVariables <<< snd) <$> p.otherParameters
      )


-- | forall a b c .
data SchemeStart tvar = SchemeStart'
  { _forall :: SourceInfo
  , typeArguments :: NonEmpty (SourceInfo, tvar)
  , dot :: SourceInfo
  }
  deriving (Show, Eq, Ord)


data DefinitionTypeAnnotation evar tvar = DefinitionTypeAnnotation'
  { colon :: SourceInfo
  , schemeStart :: Maybe (SchemeStart tvar)
  , parameters :: Maybe (Parameters evar tvar)
  , outputType :: Type tvar
  }
  deriving (Show, Eq, Ord)


-- | Either a Let definition or a Top level definition
data Definition evar tvar = Definition'
  { name :: (SourceInfo, evar)
  , _type :: Maybe (DefinitionTypeAnnotation evar tvar)
  , equal :: SourceInfo
  , definition :: Expression evar tvar
  }
  deriving (Show, Eq, Ord)


data Expression evar tvar
  = EInt {info :: SourceInfo, intValue :: Text}
  | EBool {info :: SourceInfo, boolValue :: Bool}
  | Variable {info :: SourceInfo, name :: evar}
  | Parens
      { lparen :: SourceInfo
      , expression :: Expression evar tvar
      , rparen :: SourceInfo
      }
  | EFunction
      { start :: SourceInfo
      , parameters :: Parameters evar tvar
      , body :: Expression evar tvar
      }
  | Application
      { applicationFunction :: Expression evar tvar
      , applicationRemain :: NonEmpty (Expression evar tvar)
      }
  | If
      { _if :: SourceInfo
      , condition :: Expression evar tvar
      , _then :: SourceInfo
      , ifTrue :: Expression evar tvar
      , _else :: SourceInfo
      , ifFalse :: Expression evar tvar
      }
  | Let
      { _let :: SourceInfo
      , -- The alone info is the semicolon finishing a definition
        definitions :: NonEmpty (Definition evar tvar, SourceInfo)
      , _in :: SourceInfo
      , expression :: Expression evar tvar
      }
  | Annotation
      { expression :: Expression evar tvar
      , colon :: SourceInfo
      , _type :: Type tvar
      }
  deriving (Show, Eq, Ord)
