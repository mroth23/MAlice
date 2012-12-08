module MAlice.Optimisation.ASTOptimiser where

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

optimiseExpr :: Expr -> Expr
optimiseExpr = id

optimiseAPs :: ActualParams -> ActualParams
optimiseAPs (APList a) =
  APList (map optimiseExpr a)