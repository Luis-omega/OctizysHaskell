module Repl.Ast (ReplCommand (Quit), ReplTop (Evaluate, Define, Command)) where

import Ast (Expression, TopItem)

data ReplCommand = Quit

data ReplTop
  = Evaluate Expression
  | Define TopItem
  | Command ReplCommand
