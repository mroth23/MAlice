module MAlice.Interactive.Types where

import Control.Monad.State
import MAlice.Language.AST
import MAlice.Language.SymbolTable

data UserInput =
  EvalStmt CompoundStmt |
  EvalExpr Expr         |
  Command UserCommand
  deriving (Eq, Show)

data UserCommand =
  LoadFile String |
  ReloadFile      |
  Quit
  deriving (Eq, Show)

type MEval a = StateT RuntimeState IO a

data RuntimeState = RuntimeState
                    { symbolTables :: [SymbolTable] }
