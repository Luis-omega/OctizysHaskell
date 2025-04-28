{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Octizys.Effects.Parser.Interpreter(runParser) where

import Octizys.Effects.Parser.Effect
  ( Parser (CatchParseError, GetParseState, PutParseState, ThrowParseError)
  , ParserError
  , ParserState
  )

import Effectful (Eff, (:>))
import Effectful.Dispatch.Dynamic (interpret, localSeqUnlift)
import Effectful.Error.Static (Error, catchError, throwError)
import Effectful.State.Static.Local (State, get, put)
import Control.Arrow ((<<<))


runParser
  :: ( State ParserState :> es
     , Error (ParserError e) :> es
     )
  => Eff (Parser e : es) a
  -> Eff es a
runParser = interpret $ \env action ->
  case action of
    ThrowParseError err -> throwError err
    CatchParseError p handler ->
      catchError
        ( localSeqUnlift
            env
            ( \locallyRun ->
                locallyRun p
            )
        )
        ( \_ err -> localSeqUnlift env $
            \locallyRun -> (locallyRun <<< handler) err
        )
    GetParseState -> get
    PutParseState s -> put s

