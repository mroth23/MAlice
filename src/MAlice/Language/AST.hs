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
  deriving (Eq)

instance Show Decl where
  show (VarDecl t ident) =
    ident ++ " was a " ++ (show t)
  show (VAssignDecl t ident expr) =
    ident ++ " was a " ++ (show t) ++ " of " ++ (show expr)
  show (VArrayDecl t ident expr) =
    ident ++ " had " ++ (show expr) ++ " " ++ (show t)
  show (FuncDecl ident params t body) =
    "The room " ++ ident ++ (show params) ++ " contained a " ++ (show t)
  show (ProcDecl ident params body) =
    "The looking-glass " ++ ident ++ (show params)

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
  deriving (Eq)

instance Show Expr where
  show (EPlus e1 e2) = show e1 ++ " + " ++ show e2
  show (EMinus e1 e2) = show e1 ++ " - " ++ show e2
  show (EMult e1 e2) = show e1 ++ " * " ++ show e2
  show (EDiv e1 e2) = show e1 ++ " / " ++ show e2
  show (EMod e1 e2) = show e1 ++ " % " ++ show e2
  show (EBAnd e1 e2) = show e1 ++ " & " ++ show e2
  show (EBOr e1 e2) = show e1 ++ " | " ++ show e2
  show (EBXor e1 e2) = show e1 ++ " ^ " ++ show e2
  show (ELAnd e1 e2) = show e1 ++ " && " ++ show e2
  show (ELOr e1 e2) = show e1 ++ "||" ++ show e2
  show (EGT e1 e2) = show e1 ++ " > " ++ show e2
  show (EGTE e1 e2) = show e1 ++ " >= " ++ show e2
  show (EEq e1 e2) = show e1 ++ " == " ++ show e2
  show (ELTE e1 e2) = show e1 ++ " <= " ++ show e2
  show (ELT e1 e2) = show e1 ++ " < " ++ show e2
  show (ENEq e1 e2) = show e1 ++ " != " ++ show e2
  show (ENot e1) = "!" ++ show e1
  show (EInv e1) = "~" ++ show e1
  show (EId ident) = ident
  show (EString str) = show str
  show (EInt int) = show int
  show (EChar c) = show c
  show (EArrRef ident e1) = ident ++ "[" ++ show e1 ++ "]"
  show (EBkt e1) = "(" ++ show e1 ++ ")"
  show (ENegate e1) = "-(" ++ show e1 ++ ")"
  show (EPositive e1) = "+(" ++ show e1 ++ ")"
  show (ECall ident params) = ident ++ "(" ++ show params ++ ")"

data ActualParams =
  APList [Expr]
  deriving (Eq)

instance Show ActualParams where
  show (APList []) = ""
  show (APList es) =
    concatMap ((++ ", ") . show) (init es) ++ (show . last $ es)

type Ident = String
type IntLiteral = Integer