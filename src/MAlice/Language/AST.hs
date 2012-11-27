module MAlice.Language.AST where

import MAlice.Language.Types

data Program =
  Program Decls
  deriving (Eq, Show)

data Decls =
  DeclList [Decl]
  deriving (Eq, Show)

data Decl =
  VarDecl Type Ident |
  VAssignDecl Type Ident Expr |
  VArrayDecl Type Ident Expr |
  FuncDecl Ident FormalParams Type Body |
  ProcDecl Ident FormalParams Body
  deriving (Eq, Show)

data FormalParams =
  FPList [FormalParam]
  deriving (Eq, Show)

data FormalParam =
  Param Type Ident
  deriving (Eq, Show)

data Body =
  DeclBody Decls CompoundStmt |
  StmtBody CompoundStmt |
  EmptyBody
  deriving (Eq, Show)

data CompoundStmt =
  CSList [Stmt]
  deriving (Eq, Show)

data Stmt =
  SBody Body |
  SNull |
  SAssign Expr Expr |
  SInc Expr |
  SDec Expr |
  SReturn Expr |
  SPrint Expr |
  SInput Expr |
  SCall Ident ActualParams |
  SLoop Expr CompoundStmt |
  SIf [IfClause]
  deriving (Eq, Show)

data IfClause =
  If Expr CompoundStmt |
  Else CompoundStmt
  deriving (Eq, Show)

data Expr =
  EPlus Expr Expr |
  EMinus Expr Expr |
  EMult Expr Expr |
  EDiv Expr Expr |
  EMod Expr Expr |
  EBAnd Expr Expr |
  EBOr Expr Expr |
  EBXor Expr Expr |
  ELOr Expr Expr |
  ELAnd Expr Expr |
  EGT Expr Expr |
  EGTE Expr Expr |
  EEq Expr Expr |
  ELTE Expr Expr |
  ELT Expr Expr |
  ENEq Expr Expr |
  ENot Expr |
  EInv Expr |
  EId Ident |
  EString String |
  EInt IntLiteral |
  EChar Char |
  EArrRef Ident Expr |
  EBkt Expr |
  ECall Ident ActualParams |
  ENegate Expr |
  EPositive Expr
  deriving (Eq, Show)

data ActualParams =
  APList [Expr]
  deriving (Eq, Show)

type Ident = String
type IntLiteral = Integer