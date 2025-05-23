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


data ReplCommand = Quit | LoadFile Text


data ReplTop
  = Evaluate Expression.Expression
  | Define Expression.Definition
  | Command ReplCommand
