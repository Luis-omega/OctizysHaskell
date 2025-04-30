module Octizys.Repl.Ast
  ( ReplCommand (Quit, LoadFile)
  , ReplTop
    ( Evaluate
    , Define
    , Command
    )
  ) where

import Data.Text (Text)
import qualified Octizys.Cst.Expression as Expression
import qualified Octizys.Cst.TopItem as TopItem


data ReplCommand = Quit | LoadFile Text


data ReplTop
  = Evaluate Expression.Expression
  | Define TopItem.Module
  | Command ReplCommand
