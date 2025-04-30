module Octizys.Repl.Ast (ReplCommand (Quit, LoadFile), ReplTop (Evaluate, Define, Command)) where

import qualified Octizys.Parser.Expression as Expression
import qualified Octizys.Parser.TopItem as TopItem
import qualified Octizys.Parser.Type as Type


data ReplCommand = Quit | LoadFile String


data ReplTop
  = Evaluate Expression.Parser
  | Define TopItem.Parser
  | Command ReplCommand
