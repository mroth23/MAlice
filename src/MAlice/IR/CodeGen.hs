module MAlice.IR.CodeGen
       ( generateIRCode
       ) where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import MAlice.IR.Types as IR
import MAlice.Language.AST as AST
import qualified MAlice.Language.Types as MAlice

generateIRCode :: AST.Program -> Either String IR.Program
generateIRCode (AST.Program (DeclList ps)) =
  Right $ IR.Program { code = runIdentity $ evalStateT (genDecls ps) initState }

genDecls :: [Decl] -> CodeGen Code
genDecls ds = do
  globals <- generateGlobals ds
  methods <- generateMethods ds
  return $ globals ++ methods

generateGlobals :: [Decl] -> CodeGen Code
-- Base case
generateGlobals [] = return []
-- A declaration without assignment
generateGlobals ((VarDecl typ var)       : ds) = do
  vname <- globalLabel var
  insertSymbol var vname
  rest <- generateGlobals ds
  return $ [IAlloc (AId vname)] ++ rest
-- A declaration with assignment
generateGlobals ((VAssignDecl typ var e) : ds) = do
  (lbl, code) <- generateExpr e
  vname <- globalLabel var
  insertSymbol var vname
  rest <- generateGlobals ds
  return $ [IAlloc (AId vname)] ++ code ++ [ICopy (AId vname) lbl] ++ rest
-- An array declaration
generateGlobals ((VArrayDecl typ var e)  : ds) = do
  (lbl, code) <- generateExpr e
  vname <- globalLabel var
  insertSymbol var vname
  rest <- generateGlobals ds
  return $ code ++ [IAllocArr (AId vname) lbl] ++ rest
-- Other cases are dealt with in generateMethods
generateGlobals (d:ds) =
  generateGlobals ds

generateMethods :: [Decl] -> CodeGen Code
-- Base case
generateMethods [] = return []
-- Function declaration
generateMethods ((FuncDecl f ps t body) : ds) = do
  fname <- methodLabel f
  insertSymbol f fname
  paramDecls <- generateFPs fname ps
  newSymbolTable
  bodyCode <- generateBody body
  removeSymbolTable
  rest <- generateMethods ds
  return $
    [IMLabel (AId fname) (Just t)] ++ paramDecls ++ bodyCode ++ [IExit] ++ rest
-- Procedure declaration
generateMethods ((ProcDecl f ps body) : ds) = do
  fname <- methodLabel f
  insertSymbol f fname
  paramDecls <- generateFPs fname ps
  newSymbolTable
  bodyCode <- generateBody body
  rest <- generateMethods ds
  return $
    [IMLabel (AId fname) Nothing] ++ paramDecls ++ bodyCode ++ [IExit] ++ rest
-- Other cases are dealt with in generateGlobals
generateMethods (d:ds) = generateMethods ds

generateFPs :: String -> FormalParams -> CodeGen Code
generateFPs f (FPList ps) =
  mapM (generateFP f) (zip ps [0..])

generateFP :: String -> (FormalParam, Int) -> CodeGen Instr
generateFP f ((Param t var), ix) = do
  pname <- paramLabel var
  insertSymbol var pname
  return $ IAllocParam (AId pname) ix t

generateBody :: Body -> CodeGen Code
generateBody (DeclBody ds cst) = do
  dsCode <- generateDecls ds
  cstCode <- generateCompoundStmt cst
  return $ dsCode ++ cstCode
generateBody (StmtBody cst) =
  generateCompoundStmt cst
generateBody (EmptyBody) =
  return []

generateDecls :: Decls -> CodeGen Code
generateDecls (DeclList ds) =
  concat `liftM` (mapM generateDecl ds)

generateDecl :: Decl -> CodeGen Code
generateDecl (VarDecl typ var) = do
  vname <- localLabel var
  insertSymbol var vname
  return [IAlloc (AId vname)]
-- A declaration with assignment
generateDecl (VAssignDecl typ var e) = do
  (lbl, code) <- generateExpr e
  vname <- localLabel var
  insertSymbol var vname
  return $ [IAlloc (AId vname)] ++ code ++ [ICopy (AId vname) lbl]
-- An array declaration
generateDecl (VArrayDecl typ var e) = do
  (lbl, code) <- generateExpr e
  vname <- localLabel var
  insertSymbol var vname
  return $ code ++ [IAllocArr (AId vname) lbl]
-- A function declaration
generateDecl (FuncDecl f ps _ body) = do
  fname <- methodLabel f
  insertSymbol f fname
  paramDecls <- generateFPs fname ps
  bodyCode <- generateBody body
  return $ [ILabel (AId fname)] ++ paramDecls ++ bodyCode
-- A procedure declaration
generateDecl (ProcDecl f ps body) = do
  fname <- methodLabel f
  insertSymbol f fname
  paramDecls <- generateFPs fname ps
  bodyCode <- generateBody body
  return $ [ILabel (AId fname)] ++ paramDecls ++ bodyCode

generateExpr :: Expr -> CodeGen (Operand, Code)
generateExpr (EBinOp op e1 e2) = do
  (l1, e1c) <- generateExpr e1
  (l2, e2c) <- generateExpr e2
  rLbl <- uniqueLabel
  return (rLbl, e1c ++ e2c ++ [IAssignB rLbl l1 op l2])
generateExpr (EUnOp op e) = do
  (l, ec) <- generateExpr e
  rLbl <- uniqueLabel
  return (rLbl, ec ++ [IAssignU rLbl op l])
generateExpr (EId t var) = do
  vname <- getDefinition var
  return (AId vname, [])
generateExpr (EString str) = do
  return (AString str, [])
generateExpr (EInt i) = do
  return (AInt (fromInteger i), [])
generateExpr (EChar c) = do
  return (AChar c, [])
generateExpr (EArrRef t arr e) = do
  (ix, code) <- generateExpr e
  aname <- getDefinition arr
  return (AArrRef (AId aname) ix, code)
generateExpr (EBkt e) =
  generateExpr e
generateExpr (ECall t f aps@(APList as)) = do
  paramCode <- generateAPs aps
  rLbl <- uniqueLabel
  fname <- getDefinition f
  return (ACall (AId fname) (length as), paramCode)

generateAPs :: ActualParams -> CodeGen Code
generateAPs (APList aps) =
  concat `liftM` mapM generateAP aps

generateAP :: Expr -> CodeGen Code
generateAP e = do
  (lbl, code) <- generateExpr e
  return $ code ++ [IParam lbl]

generateCompoundStmt :: CompoundStmt -> CodeGen Code
generateCompoundStmt (CSList ss) =
  concat `liftM` (mapM generateStmt ss)

generateStmt :: Stmt -> CodeGen Code
generateStmt (SBody b) =
  generateBody b
generateStmt (SNull) =
  return []
generateStmt (SAssign e1 e2) = do
  (l1, code1) <- generateExpr e1
  (l2, code2) <- generateExpr e2
  return $ code1 ++ code2 ++ [ICopy l1 l2]
generateStmt (SInc e) = do
  (l, _) <- generateExpr e
  return [IAssignB l l "+" (AInt 1)]
generateStmt (SDec e) = do
  (l, _) <- generateExpr e
  return [IAssignB l l "-" (AInt 1)]
generateStmt (SReturn e) = do
  (l, code) <- generateExpr e
  return $ code ++ [IReturn l]
generateStmt (SPrint e) = do
  (l, code) <- generateExpr e
  return $ code ++ [IPrint l]
generateStmt (SInput e) = do
  (l, _) <- generateExpr e
  return [IInput l]
generateStmt (SCall f aps@(APList as)) = do
  paramCode <- generateAPs aps
  return $ paramCode ++ [ICall (AId f) (length as)]
generateStmt (SLoop expr cst) = do
  l1 <- labelLabel
  l2 <- labelLabel
  (cond, code) <- generateExpr expr
  loopBody <- generateCompoundStmt cst
  return $ [ILabel l1] ++ code ++ [ICGoto cond l2] ++
    loopBody ++ [IGoto l1] ++ [ILabel l2]
generateStmt (SIf ifs) = do
  endLbl <- labelLabel
  ifClauses <- generateIfClauses ifs endLbl
  return $ ifClauses ++ [ILabel endLbl]

generateIfClauses :: [IfClause] -> Label -> CodeGen Code
generateIfClauses ifs endlbl =
  concat `liftM` (mapM (generateIfClause endlbl) ifs)

generateIfClause :: Label ->  IfClause -> CodeGen Code
generateIfClause end (If cond cst) = do
  (cl, code) <- generateExpr cond
  ifBody <- generateCompoundStmt cst
  endIf <- labelLabel
  return $ code ++ [INCGoto cl endIf] ++ ifBody ++ [IGoto end] ++ [ILabel endIf]
generateIfClause _ (Else cst) = do
  elseBody <- generateCompoundStmt cst
  return $ elseBody