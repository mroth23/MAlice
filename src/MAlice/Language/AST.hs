module MAlice.Language.AST where

import MAlice.Language.Types

-- This module specifies the abstract syntax tree along with some nice show
-- instances used for error output. If you want the whole tree printed it is
-- advisable to use the pretty printer by calling the compiler with -p on the
-- command line

-- A Program is a list of declarations (variables and methods)
data Program =
  Program Decls
  deriving (Eq, Show)

data Decls =
  DeclList [Decl]
  deriving (Eq, Show)

-- Different types of declarations
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

-- Formal parameters of methods as they appear in their definitions
data FormalParams =
  FPList [FormalParam]
  deriving (Eq, Show)

-- just a type and an identifier that is used locally
data FormalParam =
  Param Type Ident
  deriving (Eq, Show)

-- Body of a function, or used as a block statement
-- Can either have a compound statement, or declarations
-- followed by one. It can also be empty
data Body =
  DeclBody Decls CompoundStmt |
  StmtBody CompoundStmt       |
  EmptyBody
  deriving (Eq, Show)

-- Just a list of statements
data CompoundStmt =
  CSList [Stmt]
  deriving (Eq, Show)

-- Different statements: body block, null, assignment, increment/decrement,
-- input, return, print, procedure call, loops and conditionals
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

-- Either a condition and a compound statement or just a compound statement
data IfClause =
  If Expr CompoundStmt |
  Else CompoundStmt
  deriving (Eq, Show)

-- The expression data type. Can either be an atomic value (EInt, EChar etc) or
-- be a composition of an operator with an expression (or more)
data Expr =
  EBinOp String Expr Expr         |
  EUnOp String Expr               |
  EId Type Ident                  |
  EString String                  |
  EInt IntLiteral                 |
  EChar Char                      |
  EArrRef Type Ident Expr         |
  EBool Bool                      |
  ECall Type Ident ActualParams
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
  show (ECall _ f aps)      = f ++ "(" ++ show aps ++ ")"

-- The actual parameters used in a function call, i.e. a list of expressions
data ActualParams =
  APList [Expr]
  deriving (Eq)

instance Show ActualParams where
  show (APList []) = ""
  show (APList es) =
    concatMap ((++ ", ") . show) (init es) ++ (show . last $ es)

-- Identifiers are represented as a string, integer literals are Haskell Ints
type Ident = String
type IntLiteral = Int
