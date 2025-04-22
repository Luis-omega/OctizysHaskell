module Repl.Ast (ReplCommand (Quit), ReplTop (Evaluate, Define, Command)) where

import Ast (ParserExpression, ParserTopItem)


data ReplCommand = Quit


data ReplTop
  = Evaluate ParserExpression
  | Define ParserTopItem
  | Command ReplCommand
