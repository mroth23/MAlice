module MAlice.Transformation.Rename
       ( renameIdentifiers
       ) where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import MAlice.Transformation.Types
import MAlice.Language.AST as AST
import qualified MAlice.Language.Types as MAlice

renameIdentifiers :: AST.Program -> AST.Program
renameIdentifiers (AST.Program ds) =
  Program $ runIdentity $ evalStateT (renameGlobalDecls ds) initState

renameFPs :: String -> FormalParams -> Transform FormalParams
renameFPs f (FPList ps) =
  FPList `liftM` (mapM (renameFP f) ps)

renameFP :: String -> FormalParam -> Transform FormalParam
renameFP f (Param t var) = do
  pname <- paramLabel $ f ++ "_" ++ var
  insertSymbol var pname t
  return $ Param t pname

renameBody :: Body -> Transform Body
renameBody (DeclBody ds cst) = do
  rds <- renameDecls ds
  rcst <- renameCompoundStmt cst
  return $ DeclBody rds rcst
renameBody (StmtBody cst) = do
  rcst <- renameCompoundStmt cst
  return $ StmtBody rcst
renameBody (EmptyBody) =
  return EmptyBody

renameGlobalDecls :: Decls -> Transform Decls
renameGlobalDecls (DeclList ds) =
  DeclList `liftM` (mapM renameGlobalDecl ds)

renameGlobalDecl :: Decl -> Transform Decl
renameGlobalDecl (VarDecl typ var) = do
  vname <- globalLabel var
  insertSymbol var vname typ
  return $ VarDecl typ vname
-- A declaration with assignment
renameGlobalDecl (VAssignDecl typ var e) = do
  ne <- renameExpr e
  vname <- globalLabel var
  insertSymbol var vname typ
  return $ VAssignDecl typ vname ne
-- An array declaration
renameGlobalDecl (VArrayDecl typ var e) = do
  ne <- renameExpr e
  vname <- globalLabel var
  insertSymbol var vname typ
  return $ VArrayDecl typ vname ne
-- A function declaration
renameGlobalDecl (FuncDecl f ps t body) = do
  insertSymbol f f t
  rps <- renameFPs f ps
  rbody <- renameBody body
  return $ FuncDecl f rps t rbody
-- A procedure declaration
renameGlobalDecl (ProcDecl f ps body) = do
  insertSymbol f f MAlice.Void
  rps <- renameFPs f ps
  rbody <- renameBody body
  return $ ProcDecl f rps rbody

renameDecls :: Decls -> Transform Decls
renameDecls (DeclList ds) =
  DeclList `liftM` (mapM renameDecl ds)

renameDecl :: Decl -> Transform Decl
renameDecl (VarDecl typ var) = do
  vname <- localLabel var
  insertSymbol var vname typ
  return $ VarDecl typ vname
-- A declaration with assignment
renameDecl (VAssignDecl typ var e) = do
  ne <- renameExpr e
  vname <- localLabel var
  insertSymbol var vname typ
  return $ VAssignDecl typ vname ne
-- An array declaration
renameDecl (VArrayDecl typ var e) = do
  ne <- renameExpr e
  vname <- localLabel var
  insertSymbol var vname typ
  return $ VArrayDecl typ vname ne
-- A function declaration
renameDecl (FuncDecl f ps t body) = do
  fname <- methodLabel f
  insertSymbol f fname t
  rps <- renameFPs fname ps
  rbody <- renameBody body
  return $ FuncDecl fname rps t rbody
-- A procedure declaration
renameDecl (ProcDecl f ps body) = do
  fname <- methodLabel f
  insertSymbol f fname MAlice.Void
  rps <- renameFPs fname ps
  rbody <- renameBody body
  return $ ProcDecl fname rps rbody

renameExpr :: Expr -> Transform Expr
renameExpr (EBinOp op e1 e2) = do
  re1 <- renameExpr e1
  re2 <- renameExpr e2
  return $ EBinOp op re1 re2
renameExpr (EUnOp op e) = do
  re <- renameExpr e
  return $ EUnOp op re
renameExpr (EId t var) = do
  vname <- getDefinition var
  return $ EId t vname
renameExpr (EArrRef t arr e) = do
  re <- renameExpr e
  aname <- getDefinition arr
  return $ EArrRef t aname re
renameExpr (ECall t f aps) = do
  raps <- renameAPs aps
  fname <- getDefinition f
  return $ ECall t fname raps
renameExpr e = return e

renameAPs :: ActualParams -> Transform ActualParams
renameAPs (APList aps) =
  APList `liftM` (mapM renameAP aps)

renameAP :: Expr -> Transform Expr
renameAP = renameExpr

renameCompoundStmt :: CompoundStmt -> Transform CompoundStmt
renameCompoundStmt (CSList ss) =
  CSList `liftM` (mapM renameStmt ss)

renameStmt :: Stmt -> Transform Stmt
renameStmt (SBody b) = do
  newSymbolTable
  rb <- renameBody b
  exitBlock
  return $ SBody rb
renameStmt (SNull) =
  return SNull
renameStmt (SAssign e1 e2) = do
  re1 <- renameExpr e1
  re2 <- renameExpr e2
  return $ SAssign re1 re2
renameStmt (SInc e) =
  SInc `liftM` (renameExpr e)
renameStmt (SDec e) =
  SDec `liftM` (renameExpr e)
renameStmt (SReturn e) =
  SReturn `liftM` (renameExpr e)
renameStmt (SPrint e) =
  SPrint `liftM` (renameExpr e)
renameStmt (SInput e) =
  SInput `liftM` (renameExpr e)
renameStmt (SCall f aps) = do
  raps <- renameAPs aps
  fname <- getDefinition f
  return $ SCall fname raps
renameStmt (SLoop expr cst) = do
  re <- renameExpr expr
  rcst <- renameCompoundStmt cst
  return $ SLoop re rcst
renameStmt (SIf ifs) =
  SIf `liftM` (mapM renameIfClause ifs)

renameIfClause :: IfClause -> Transform IfClause
renameIfClause (If cond cst) = do
  rc <- renameExpr cond
  rcst <- renameCompoundStmt cst
  return $ If rc rcst
renameIfClause (Else cst) =
  Else `liftM` (renameCompoundStmt cst)