module MAlice.Optimisation.ASTOptimiser where

import Data.Bits
import Data.Char
import Data.Maybe
import MAlice.Language.AST

--Repeat until AST doesn't change anymore. This is somewhat inefficient but
--should generally be fine.
optimiseAST :: Program -> Program
optimiseAST p =
  if optP == p
  then p
  else optimiseAST optP
  where
    optP = optimiseProgram p

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
  CSList $ catMaybes (optimiseStmts ss)

optimiseStmts :: [Stmt] -> [Maybe Stmt]
optimiseStmts ((SAssign a e):(SAssign b e2):ss)
  | e2 == a = (Just (SAssign b e)) : (optimiseStmts ss)
optimiseStmts (s : ss) = (optimiseStmt s) : (optimiseStmts ss)

optimiseStmt :: Stmt -> Maybe Stmt
optimiseStmt (SBody b) =
  case b of
    EmptyBody -> Nothing
    body      -> Just $ SBody (optimiseBody b)
optimiseStmt (SAssign e1 e2)
  | e1 == e2   = Nothing
  | otherwise = Just $ SAssign (optimiseExpr e1) (optimiseExpr e2)
optimiseStmt (SInc e) =
  Just $ SInc (optimiseExpr e)
optimiseStmt (SDec e) =
  Just $ SDec (optimiseExpr e)
optimiseStmt (SReturn e) =
  Just $ SReturn (optimiseExpr e)
optimiseStmt (SPrint e) =
  Just $ SPrint (optimiseExpr e)
optimiseStmt (SCall i a) =
  Just $ SCall i (optimiseAPs a)
optimiseStmt (SLoop e l) =
  Just $ SLoop (optimiseExpr e) (optimiseCompoundStmt l)
optimiseStmt (SIf c) =
  Just $ SIf $ catMaybes (map optimiseIfClause c)

optimiseIfClause :: IfClause -> Maybe IfClause
optimiseIfClause (If e c) =
  case optimiseExpr e of
    EBool False -> Nothing
    expr        -> Just $ If expr (optimiseCompoundStmt c)
optimiseIfClause (Else c) =
  Just $ Else (optimiseCompoundStmt c)

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
optimiseExpr (EBinOp "*" e (EUnOp "-" (EInt 1))) = EUnOp "-" e
optimiseExpr (EBinOp "*" (EUnOp "-" (EInt 1)) e) = EUnOp "-" e
optimiseExpr (EBinOp "/" (EInt 1) e) = e
optimiseExpr (EBinOp "/" e (EInt 1)) = e
optimiseExpr (EBinOp "/" (EInt 0) e) = EInt 0
optimiseExpr (EBinOp "/" e (EUnOp "-" (EInt 1))) = EUnOp "-" e
optimiseExpr (EBinOp "/" e1 e2)
  | e1 == e2 = EInt 1
optimiseExpr (EBinOp "&&" (EBool False) e) = EBool False
optimiseExpr (EBinOp "&&" e (EBool False)) = EBool False
optimiseExpr (EBinOp "||" (EBool True) e) = EBool True
optimiseExpr (EBinOp "||" e (EBool True)) = EBool True
optimiseExpr (EBinOp op e1 e2) =
  EBinOp op (optimiseExpr e1) (optimiseExpr e2)
optimiseExpr (EUnOp "-" (EInt i)) = EInt (negate i)
optimiseExpr (EUnOp "+" (EInt i)) = EInt i
optimiseExpr (EUnOp "~" (EInt i)) = EInt (complement i)
optimiseExpr (EUnOp "!" (EBool b)) = EBool (not b)
optimiseExpr e = e

optimiseAPs :: ActualParams -> ActualParams
optimiseAPs (APList a) =
  APList (map optimiseExpr a)