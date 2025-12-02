{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- | This module contains all the definitions needed to define the `Parser` effect.
it is unstable.
-}
module EffectfulParserCombinators.Backend
  ( Expected (ExpectedRaw, ExpectedName, ExpectedEndOfInput)
  , Unexpected (UnexpectedRaw, UnexpectedEndOfInput)
  , ParserError
    ( GeneratedError
    , UserMadeError
    , errorPosition
    , userErrors
    , unexpected
    , expected
    )
  , UserError (SimpleError, CustomError)
  , Expectations (Expectations')
  , ParserState
    ( ParserState'
    , position
    , remainStream
    , expected
    )
  , insertExpectation
  , makeInitialState
  , addExpectation
  , mergeExpectations
  , emptyExpectations
  , singletonExpectations
  , prettyUserError
  , makeParseErrorReport
  ) where

