{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module defines a `Parser` effect with basic parsing capabilities.
module EffectfulParserCombinators.Effect
  ( Parser (ThrowParseError, CatchParseError, GetParseState, PutParseState)
  , throwParseError
  , putParseState
  , getParseState
  , catchParseError
  ) where

import Effectful (Effect)
import Effectful.TH (makeEffect)
import EffectfulParserCombinators.Error (ParserError)
import EffectfulParserCombinators.ParserState (ParserState)


data Parser e :: Effect where
  ThrowParseError :: ParserError e -> Parser e m a
  CatchParseError
    :: m a
    -> (ParserError e -> m a)
    -> Parser e m a
  GetParseState :: Parser e m ParserState
  PutParseState :: ParserState -> Parser e m ()


$(makeEffect ''Parser)
