module Octizys.Repl.Ast (ReplCommand (Quit, LoadFile), ReplTop (Evaluate, Define, Command)) where

import Octizys.Ast (ParserExpression, ParserTopItem)


data ReplCommand = Quit | LoadFile String


data ReplTop
  = Evaluate ParserExpression
  | Define ParserTopItem
  | Command ReplCommand
