{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module defines a `Parser` effect with basic parsing capabilities.
module Octizys.Effects.Parser.Effect
  ( Parser (ThrowParseError, CatchParseError, GetParseState, PutParseState)
  , throwParseError
  , putParseState
  , getParseState
  , catchParseError
  ) where

import Effectful (Effect)
import Effectful.TH (makeEffect)
import Octizys.Effects.Parser.Backend (ParserError, ParserState)


-- | Effect for generating fresh integers with phantom types
data Parser e :: Effect where
  ThrowParseError :: ParserError e -> Parser e m a
  CatchParseError
    :: m a
    -> (ParserError e -> m a)
    -> Parser e m a
  GetParseState :: Parser e m ParserState
  PutParseState :: ParserState -> Parser e m ()


$(makeEffect ''Parser)

