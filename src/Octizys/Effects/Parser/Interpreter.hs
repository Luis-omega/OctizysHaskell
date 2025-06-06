{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Octizys.Effects.Parser.Interpreter (runParser, runFullParser, runParserWith, ParserState) where

import Octizys.Effects.Parser.Backend
  ( ParserError
  , ParserState
  , makeInitialState
  )
import Octizys.Effects.Parser.Effect
  ( Parser (CatchParseError, GetParseState, PutParseState, ThrowParseError)
  )

import Control.Arrow ((<<<))
import Data.Text (Text)
import Effectful (Eff, (:>))
import Effectful.Dispatch.Dynamic (interpret, localSeqUnlift)
import Effectful.Error.Static
  ( Error
  , catchError
  , runErrorNoCallStack
  , throwError
  )
import Effectful.State.Static.Local (State, get, put, runState)


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


runParserWith
  :: Error (ParserError e) :> es
  => ParserState
  -> Eff
      ( Parser e
          : State ParserState
          : es
      )
      a
  -> Eff es (a, ParserState)
runParserWith s p = runState s $ runParser p


runFullParser
  :: Text
  -> Eff
      ( Parser e
          : State ParserState
          : Error (ParserError e)
          : es
      )
      a
  -> Eff es (Either (ParserError e) a)
runFullParser stream p = do
  let s = makeInitialState stream
  ( runErrorNoCallStack
      <<< (fst <$>)
      <<< runState s
      <<< runParser
    )
    p
