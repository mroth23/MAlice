module MAlice.Transformation.Annotate
       (annotateIdentifiers)
       where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Data.List
import Data.Maybe
import MAlice.Transformation.Types
import MAlice.Language.AST as AST
import MAlice.Language.Types

freeInDecls :: ADecls -> FreeVars
freeInDecls ds = nub $ concatMap freeInDecl ds

freeInDecl :: ADecl -> FreeVars
freeInDecl (AVarDecl _ _)         = []
freeInDecl (AVAssignDecl _ _ _ f) = f
freeInDecl (AVArrayDecl _ _ _ f)  = f
freeInDecl (AFuncDecl _ _ _ _ f)  = f
freeInDecl (AProcDecl _ _ _ f)    = f

freeInCompoundStmt :: ACompoundStmt -> FreeVars
freeInCompoundStmt (ACSList cst) = nub $ concatMap freeInStmt cst

freeInStmt :: AStmt -> FreeVars
freeInStmt (ASBody _ f)     = f
freeInStmt (ASNull)         = []
freeInStmt (ASAssign _ _ f) = f
freeInStmt (ASInc _ f)      = f
freeInStmt (ASDec _ f)      = f
freeInStmt (ASReturn _ f)   = f
freeInStmt (ASPrint _ f)    = f
freeInStmt (ASInput _ f)    = f
freeInStmt (ASCall _ _ f)   = f
freeInStmt (ASLoop _ _ f)   = f
freeInStmt (ASIf _ f)       = f

freeInExpr :: AExpr -> FreeVars
freeInExpr (AEBinOp _ _ _ f)  = f
freeInExpr (AEUnOp _ _ f)     = f
freeInExpr (AEId _ _ f)       = f
freeInExpr (AEString _)       = []
freeInExpr (AEInt _)          = []
freeInExpr (AEChar _)         = []
freeInExpr (AEArrRef _ _ _ f) = f
freeInExpr (AEBool _)         = []
freeInExpr (AECall _ _ _ f)   = f

freeInAPs :: AActualParams -> FreeVars
freeInAPs (AAPList aps) = nub $ concatMap freeInExpr aps

freeInBody :: ABody -> FreeVars
freeInBody (AEmptyBody) = []
freeInBody (AStmtBody _ f) = f
freeInBody (ADeclBody _ _ f) = f

freeInIfs :: [AIfClause] -> FreeVars
freeInIfs ifs = nub $ concatMap freeInIf ifs

freeInIf :: AIfClause -> FreeVars
freeInIf (AIf _ _ f) = f
freeInIf (AElse _ f) = f

annotateIdentifiers :: AST.Program -> ADecls
annotateIdentifiers (AST.Program (DeclList ds)) =
  runIdentity $ evalStateT (annotateDecls ds) initState

annotateDecls :: [Decl] -> Transform ADecls
annotateDecls ds =
  mapM annotateDecl ds

annotateDecl :: Decl -> Transform ADecl
annotateDecl (VarDecl typ var) = do
  insertSymbol var var typ
  return $ AVarDecl typ var
-- A declaration with assignment
annotateDecl (VAssignDecl typ var e) = do
  e' <- annotateExpr e
  insertSymbol var var typ
  return $ AVAssignDecl typ var e' (freeInExpr e')
-- An array declaration
annotateDecl (VArrayDecl typ var e) = do
  e' <- annotateExpr e
  insertSymbol var var typ
  return $ AVArrayDecl typ var e' (freeInExpr e')
-- A function declaration
annotateDecl (FuncDecl f ps t body) = do
  insertSymbol f f t
  enterBlock ps
  abody <- annotateBody body
  exitBlock
  return $ AFuncDecl f ps t abody (freeInBody abody)
-- A procedure declaration
annotateDecl (ProcDecl f ps body) = do
  insertSymbol f f Unknown
  enterBlock ps
  abody <- annotateBody body
  exitBlock
  return $ AProcDecl f ps abody (freeInBody abody)

annotateExpr :: Expr -> Transform AExpr
annotateExpr (EBinOp op e1 e2) = do
  re1 <- annotateExpr e1
  re2 <- annotateExpr e2
  let f1 = freeInExpr re1
      f2 = freeInExpr re2
  return $ AEBinOp op re1 re2 (union f1 f2)
annotateExpr (EUnOp op e) = do
  re <- annotateExpr e
  return $ AEUnOp op re (freeInExpr re)
annotateExpr (EId t var) = do
  freeV <- isFreeVariable var
  if freeV
    then return $ AEId t var [(var, fromJust t)]
    else return $ AEId t var []
annotateExpr (EArrRef t arr e) = do
  re <- annotateExpr e
  freeV <- isFreeVariable arr
  let rfs = freeInExpr re
  if freeV
    then return $ AEArrRef t arr re ((arr, RefType . fromJust $ t):rfs)
    else return $ AEArrRef t arr re rfs
annotateExpr (ECall t f aps) = do
  aaps <- annotateAPs aps
  return $ AECall t f aaps (freeInAPs aaps)
annotateExpr (EString s) = return $ AEString s
annotateExpr (EInt s)    = return $ AEInt s
annotateExpr (EChar s)   = return $ AEChar s
annotateExpr (EBool s)   = return $ AEBool s

annotateBody :: Body -> Transform ABody
annotateBody (DeclBody (DeclList ds) cst) = do
  rds <- annotateDecls ds
  rcst <- annotateCompoundStmt cst
  let fds = freeInDecls rds
      fcs = freeInCompoundStmt rcst
  return $ ADeclBody rds rcst (union fds fcs)
annotateBody (StmtBody cst) = do
  rcst <- annotateCompoundStmt cst
  let fcs = freeInCompoundStmt rcst
  return $ AStmtBody rcst fcs
annotateBody (EmptyBody) =
  return AEmptyBody

annotateAPs :: ActualParams -> Transform AActualParams
annotateAPs (APList aps) =
  AAPList `liftM` (mapM annotateAP aps)

annotateAP :: Expr -> Transform AExpr
annotateAP = annotateExpr

annotateCompoundStmt :: CompoundStmt -> Transform ACompoundStmt
annotateCompoundStmt (CSList ss) =
  ACSList `liftM` (mapM annotateStmt ss)

annotateStmt :: Stmt -> Transform AStmt
annotateStmt (SBody b) = do
  ab <- annotateBody b
  let f = freeInBody ab
  return $ ASBody ab f
annotateStmt (SNull) =
  return ASNull
annotateStmt (SAssign e1 e2) = do
  re1 <- annotateExpr e1
  re2 <- annotateExpr e2
  let f1 = freeInExpr re1
      f2 = freeInExpr re2
  return $ ASAssign re1 re2 (union f1 f2)
annotateStmt (SInc e) = do
  ae <- annotateExpr e
  let f = freeInExpr ae
  return $ ASInc ae f
annotateStmt (SDec e) = do
  ae <- annotateExpr e
  let f = freeInExpr ae
  return $ ASDec ae f
annotateStmt (SReturn e) = do
  ae <- annotateExpr e
  let f = freeInExpr ae
  return $ ASReturn ae f
annotateStmt (SPrint e) = do
  ae <- annotateExpr e
  let f = freeInExpr ae
  return $ ASPrint ae f
annotateStmt (SInput e) = do
  ae <- annotateExpr e
  let f = freeInExpr ae
  return $ ASInput ae f
annotateStmt (SCall f aps) = do
  aaps <- annotateAPs aps
  let fv = freeInAPs aaps
  return $ ASCall f aaps fv
annotateStmt (SLoop expr cst) = do
  re <- annotateExpr expr
  rcst <- annotateCompoundStmt cst
  let f1 = freeInExpr re
      f2 = freeInCompoundStmt rcst
  return $ ASLoop re rcst (union f1 f2)
annotateStmt (SIf ifs) = do
  aifs <- mapM annotateIfClause ifs
  let f = freeInIfs aifs
  return $ ASIf aifs f

annotateIfClause :: IfClause -> Transform AIfClause
annotateIfClause (If cond cst) = do
  rc <- annotateExpr cond
  rcst <- annotateCompoundStmt cst
  let f1 = freeInExpr rc
      f2 = freeInCompoundStmt rcst
  return $ AIf rc rcst (union f1 f2)
annotateIfClause (Else cst) = do
  rcst <- annotateCompoundStmt cst
  let f = freeInCompoundStmt rcst
  return $ AElse rcst f