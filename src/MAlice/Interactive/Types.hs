module MAlice.Interactive.Types where

data UserInput =
  EvalStmt CompoundStmt |
  EvalExpr Expr         |
  Command UserCommand

data UserCommand =
  LoadFile String |
  ReloadFile      |
  Quit

type MEval a = StateT RuntimeState IO a

data RuntimeState = RuntimeState
                    { symbolTables :: [SymbolTable]
                                 }