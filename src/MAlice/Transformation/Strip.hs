module MAlice.Transformation.Strip
       ( rstrip
       , strip
       ) where

import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe
import MAlice.Language.AST

--This module strips declared but unused methods and variables from the code.
--Only used after everything has a unique name. Otherwise things may go horribly
--wrong. Recursive functions that are otherwise unused will not be detected by
--this program.

rstrip :: Program -> Program
rstrip p =
  if optP == p
  then p
  else strip optP
  where
    optP = strip p

strip :: Program -> Program
strip p@(Program (DeclList ds)) =
  evalState (removeUnused p) st'
  where
    st' = execState (annotateDecls ds) M.empty

type UseTable = M.Map String Bool

type St a = State UseTable a

putVar :: String -> Bool -> St ()
putVar k e =
  M.insert k e `liftM` get >>= put

getVar :: String -> St Bool
getVar k =
  (fromJust . M.lookup k) `liftM` get

removeUnused :: Program -> St Program
removeUnused (Program (DeclList ds)) = do
  nds <- removeDecls ds
  return . Program . DeclList $ nds

removeDecls :: [Decl] -> St [Decl]
removeDecls ds = filterM declInUse ds >>= mapM removeInMethods

declInUse :: Decl -> St Bool
declInUse (VarDecl _ var) =
  getVar var
declInUse (VAssignDecl _ var _) =
  getVar var
declInUse (VArrayDecl _ var _) =
  getVar var
declInUse (FuncDecl f _ _ _) =
  getVar f
declInUse (ProcDecl f _ _) =
  getVar f

removeInMethods :: Decl -> St Decl
removeInMethods (FuncDecl f ps t body) = do
  nb <- removeInBody body
  return $ FuncDecl f ps t nb
removeInMethods (ProcDecl f ps body) = do
  nb <- removeInBody body
  return $ ProcDecl f ps nb
removeInMethods d = return d

removeInBody :: Body -> St Body
removeInBody (DeclBody (DeclList ds) (CSList cst)) = do
  nds <- removeDecls ds
  ncst <- removeCSt cst
  return $ DeclBody (DeclList nds) (CSList ncst)
removeInBody (StmtBody (CSList cst)) = do
  ncst <- removeCSt cst
  return $ StmtBody (CSList ncst)
removeInBody b = return b

removeCSt :: [Stmt] -> St [Stmt]
removeCSt = mapM removeStmt

removeStmt :: Stmt -> St Stmt
removeStmt (SBody b) = do
  nb <- removeInBody b
  return $ SBody nb
removeStmt s = return s

annotateDecls :: [Decl] -> St ()
annotateDecls ds =
  mapM_ annotateDecl ds

annotateDecl :: Decl -> St ()
annotateDecl (VarDecl _ var) = do
  putVar var False
annotateDecl (VAssignDecl _ var e) = do
  putVar var False
  annotateExpr e
annotateDecl (VArrayDecl _ var e) = do
  putVar var False
  annotateExpr e
annotateDecl (FuncDecl f _ _ body) = do
  putVar f False
  annotateBody body
annotateDecl (ProcDecl f _ body) = do
  putVar f False
  annotateBody body

annotateExpr :: Expr -> St ()
annotateExpr (EBinOp _ e1 e2) = do
  annotateExpr e1
  annotateExpr e2
annotateExpr (EUnOp _ e) =
  annotateExpr e
annotateExpr (EId _ var) =
  putVar var True
annotateExpr (EArrRef _ arr e) = do
  annotateExpr e
  putVar arr True
annotateExpr (ECall _ f aps) = do
  annotateAPs aps
  putVar f True
annotateExpr _ =
  return ()

annotateBody :: Body -> St ()
annotateBody (DeclBody (DeclList ds) cst) = do
  annotateDecls ds
  annotateCompoundStmt cst
annotateBody (StmtBody cst) = do
  annotateCompoundStmt cst
annotateBody (EmptyBody) =
  return ()

annotateAPs :: ActualParams -> St ()
annotateAPs (APList aps) =
  mapM_ annotateAP aps

annotateAP :: Expr -> St ()
annotateAP = annotateExpr

annotateCompoundStmt :: CompoundStmt -> St ()
annotateCompoundStmt (CSList ss) =
  mapM_ annotateStmt ss

annotateStmt :: Stmt -> St ()
annotateStmt (SBody b) =
  annotateBody b
annotateStmt (SNull) =
  return ()
annotateStmt (SAssign e1 e2) = do
  annotateExpr e1
  annotateExpr e2
annotateStmt (SInc e) =
  annotateExpr e
annotateStmt (SDec e) =
  annotateExpr e
annotateStmt (SReturn e) =
  annotateExpr e
annotateStmt (SPrint e) =
  annotateExpr e
annotateStmt (SInput e) =
  annotateExpr e
annotateStmt (SCall f aps) = do
  annotateAPs aps
  putVar f True
annotateStmt (SLoop expr cst) = do
  annotateExpr expr
  annotateCompoundStmt cst
annotateStmt (SIf ifs) = do
  mapM_ annotateIfClause ifs

annotateIfClause :: IfClause -> St ()
annotateIfClause (If cond cst) = do
  annotateExpr cond
  annotateCompoundStmt cst
annotateIfClause (Else cst) =
  annotateCompoundStmt cst