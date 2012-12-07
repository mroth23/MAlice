module MAlice.Language.AST where

import MAlice.Language.Types

data Program =
  Program Decls
  deriving (Eq, Show)

data Decls =
  DeclList [Decl]
  deriving (Eq, Show)

data Decl =
  VarDecl Type Ident                    |
  VAssignDecl Type Ident Expr           |
  VArrayDecl Type Ident Expr            |
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
  StmtBody CompoundStmt       |
  EmptyBody
  deriving (Eq, Show)

data CompoundStmt =
  CSList [Stmt]
  deriving (Eq, Show)

data Stmt =
  SBody Body               |
  SNull                    |
  SAssign Expr Expr        |
  SInc Expr                |
  SDec Expr                |
  SReturn Expr             |
  SPrint Expr              |
  SInput Expr              |
  SCall Ident ActualParams |
  SLoop Expr CompoundStmt  |
  SIf [IfClause]
  deriving (Eq, Show)

data IfClause =
  If Expr CompoundStmt |
  Else CompoundStmt
  deriving (Eq, Show)

data Expr =
  EBinOp String Expr Expr         |
  EUnOp String Expr               |
  EId (Maybe Type) Ident          |
  EString String                  |
  EInt IntLiteral                 |
  EChar Char                      |
  EArrRef (Maybe Type) Ident Expr |
  EBool Bool                      |
  --there is no way to construct Booleans in MAlice source
  EBkt Expr                       |
  ECall (Maybe Type) Ident ActualParams
  deriving (Eq)

instance Show Expr where
  show (EBinOp op e1 e2)    = show e1 ++ " " ++ op ++ " " ++ show e2
  show (EUnOp op e1)        = op ++ "(" ++ show e1 ++ ")"
  show (EId _ ident)        = ident
  show (EString str)        = show str
  show (EInt int)           = show int
  show (EChar c)            = show c
  show (EBool b)            = show b
  show (EArrRef _ ident e1) = ident ++ "'s (" ++ show e1 ++ ") piece"
  show (EBkt e1)            = "(" ++ show e1 ++ ")"
  show (ECall _ f aps)      = f ++ "(" ++ show aps ++ ")"

data ActualParams =
  APList [Expr]
  deriving (Eq)

instance Show ActualParams where
  show (APList []) = ""
  show (APList es) =
    concatMap ((++ ", ") . show) (init es) ++ (show . last $ es)

type Ident = String
type IntLiteral = Integer