module Octizys.Repl.Ast (ReplCommand (Quit, LoadFile), ReplTop (Evaluate, Define, Command)) where

import qualified Octizys.Parser as Parser


data ReplCommand = Quit | LoadFile String


data ReplTop
  = Evaluate Parser.Expression
  | Define Parser.TopItem
  | Command ReplCommand
