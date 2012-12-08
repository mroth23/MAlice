module MAlice.Optimisation.ASTOptimiser where

import Data.Bits
import Data.Char
import MAlice.Language.AST

optimiseAST :: Program -> Program
optimiseAST = id

optimiseProgram :: Program -> Program
optimiseProgram (Program ds) =
  Program (optimiseDecls ds)

optimiseDecls :: Decls -> Decls
optimiseDecls (DeclList ds) =
  DeclList (map optimiseDecl ds)

optimiseDecl :: Decl -> Decl
optimiseDecl (VAssignDecl t i e) =
  VAssignDecl t i (optimiseExpr e)
optimiseDecl (VArrayDecl t i e) =
  VArrayDecl t i (optimiseExpr e)
optimiseDecl (FuncDecl i a t b) =
  FuncDecl i a t (optimiseBody b)
optimiseDecl (ProcDecl i a b) =
  ProcDecl i a (optimiseBody b)
optimiseDecl d = d

optimiseBody :: Body -> Body
optimiseBody (DeclBody ds cst) =
  DeclBody (optimiseDecls ds) (optimiseCompoundStmt cst)
optimiseBody (StmtBody cst) =
  StmtBody (optimiseCompoundStmt cst)
optimiseBody b = b

optimiseCompoundStmt :: CompoundStmt -> CompoundStmt
optimiseCompoundStmt (CSList ss) =
  CSList (map optimiseStmt ss)

optimiseStmt :: Stmt -> Stmt
optimiseStmt (SBody b) =
  SBody (optimiseBody b)
optimiseStmt (SAssign e1 e2) =
  SAssign (optimiseExpr e1) (optimiseExpr e2)
optimiseStmt (SInc e) =
  SInc (optimiseExpr e)
optimiseStmt (SDec e) =
  SDec (optimiseExpr e)
optimiseStmt (SReturn e) =
  SReturn (optimiseExpr e)
optimiseStmt (SPrint e) =
  SPrint (optimiseExpr e)
optimiseStmt (SCall i a) =
  SCall i (optimiseAPs a)
optimiseStmt (SLoop e l) =
  SLoop (optimiseExpr e) (optimiseCompoundStmt l)
optimiseStmt (SIf c) =
  SIf (map optimiseIfClause c)

optimiseIfClause :: IfClause -> IfClause
optimiseIfClause (If e c) =
  If (optimiseExpr e) (optimiseCompoundStmt c)
optimiseIfClause (Else c) =
  Else (optimiseCompoundStmt c)

intBinOps =
  [ ("+", (+))
  , ("-", (-))
  , ("*", (*))
  , ("/", div)
  , ("%", mod)
  , ("&", (.&.))
  , ("|", (.|.))
  , ("^", xor) ]

isIntBinOp  = flip elem (map fst intBinOps)

intRelOps =
  [ ("<", (<))
  , (">", (>))
  , (">=", (>=))
  , ("<=", (<=)) ]

isIntRelOp = flip elem (map fst intRelOps)

eqOps :: Eq a => [(String, a -> a -> Bool)]
eqOps =
  [ ("==", (==))
  , ("!=", (/=)) ]

isEqOp :: String -> Bool
isEqOp = flip elem ["==", "!="]

boolOps =
  [ ("&&", (&&))
  , ("||", (||)) ]

isBoolOp = flip elem (map fst boolOps)

optimiseExpr :: Expr -> Expr
-- Constant folding
-- -----------------------------------------------------------------------------
optimiseExpr (EBinOp op (EInt a) (EInt b))
  | isIntBinOp op = let (Just o) = lookup op intBinOps in EInt  (o a b)
  | isIntRelOp op = let (Just o) = lookup op intRelOps in EBool (o a b)
  | isEqOp op     = let (Just o) = lookup op eqOps     in EBool (o a b)
  -- Otherwise type error, and will be caught by the type checker
optimiseExpr (EBinOp op (EChar a) (EChar b))
  | isEqOp op     = let (Just o) = lookup op eqOps     in EBool (o a b)
  | isIntBinOp op = let (Just o) = lookup op intBinOps in EChar $ chr (o (ord a) (ord b))
optimiseExpr (EBinOp op (EBool a) (EBool b))
  | isBoolOp op   = let (Just o) = lookup op boolOps   in EBool (o a b)
-- -----------------------------------------------------------------------------
-- Arithmetic identities
optimiseExpr (EBinOp "+" (EInt 0) e) = e
optimiseExpr (EBinOp "+" e (EInt 0)) = e
optimiseExpr (EBinOp "-" (EInt 0) e) = e
optimiseExpr (EBinOp "-" e (EInt 0)) = e
optimiseExpr (EBinOp "*" (EInt 0) e) = EInt 0
optimiseExpr (EBinOp "*" e (EInt 0)) = EInt 0
optimiseExpr (EBinOp "*" (EInt 1) e) = e
optimiseExpr (EBinOp "*" e (EInt 1)) = e
optimiseExpr (EBinOp "/" (EInt 1) e) = e
optimiseExpr (EBinOp "/" e (EInt 1)) = e
optimiseExpr (EBinOp "/" (EInt 0) e) = EInt 0
optimiseExpr (EBinOp op e1 e2) =
  EBinOp op (optimiseExpr e1) (optimiseExpr e2)
optimiseExpr e = e

optimiseAPs :: ActualParams -> ActualParams
optimiseAPs (APList a) =
  APList (map optimiseExpr a)