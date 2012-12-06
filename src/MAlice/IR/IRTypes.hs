module MAlice.IR.Types where

import qualified MAlice.Types as MAlice
import qualified MAlice.Language.AST as AST
import MAlice.Language.Types

import qualified Data.Map as M
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State

newtype IRProgram = IRProgram { code :: IRCode }

showProgram :: IRProgram -> IO ()
showProgram irp = mapM_ print $ code irp

type IRCode = [Instr]

type CodeGen a = StateT CState Identity a
type SymbolTable = [SymbolTableEntry]
type SymbolTableEntry = M.Map String String

data CState = CState { lblCount :: Int
                     , symTables :: [SymbolTable] }

initState = CState { lblCount = 0 }

getSymbolTables :: CodeGen [SymbolTable]
getSymbolTables =
  symTables `liftM` get

insertSymbol :: String -> String -> CodeGen ()
insertSymbol k e = do
  (t:ts) <- getSymbolTables
  let newt = M.insert k e t
  updateState $ \st -> st { symTables = newt : ts }

newSymbolTable :: CodeGen ()
newSymbolTable = do
  ts <- getSymbolTables
  updateState $ \st -> st { symTables = M.empty : ts }

removeSymbolTable :: CodeGen ()
removeSymbolTable = do
  (t : ts) <- getSymbolTables
  updateState $ \st -> st { symTables = ts }

getDefinition :: String -> MParser String
getDefinition v = (fromJust . lookupInTables v) `liftM` getSymbolTables

lookupInTables :: String -> [SymbolTable] -> Maybe String
lookupInTables _ [] = Nothing
lookupInTables s (t : ts) =
  case M.lookup s t of
    Nothing -> lookupInTables s ts
    Just e -> Just e

globalPrefix = "__global_"
paramPrefix = "__param_"
localPrefix = "__var_"
methodPrefix = "__m_"
labelPrefix = "__"

uniqueNumber = do
  rval <- lblCount `liftM` get
  updateState $ \st -> st { lblCount = rval + 1 }
  return rval

uniqueLabel = do
  rval <- uniqueNumber
  return $ EId ("__t" ++ rval)

data Instr =
  IAlloc Label                          | --Allocate a new variable
  IAllocArr Label Operand               | --Allocate an array
  IAllocParam Label Int MAlice.Type     | --Allocate a function param
  IAssignB Label Operand String Operand | --x := y `op` z
  IAssignU Label String Operand         | --x := op y
  ICopy Label Label                     | --x := y
  ICall Label Int                       | --x := y(..) with n params
  IGoto Label                           | --goto Label
  ICGoto Label Label                    | --if a goto label
  INCGoto Label Label                   | --if !a goto label
  ILabel Label                          | --Label:
  IMLabel Label                         | --Method label
  IParam Label                          | --Use label as param in call
  IReturn Label                         | --Return label
  IPrint Label                          | --print label
  IExit                                 |
  IInput Label

type Label = Operand

data Operand =
  AId String              | --id/label
  AString String          | --String literal
  AInt Int                | --x := Int literal
  AChar Char              | --x := Char literal
  AArrRef Label Operand   | --a[ix]
  ACall Label Int           --f() with x params

generateIRCode :: Program -> Either String IRProgram
generateIRCode (DeclList ps) =
  Right . IRProgram . runIdentity $ evalStateT (genDecls ps) initState

genDecls :: [Decl] -> CodeGen IRCode
genDecls ds = do
  globals <- generateGlobals ds
  methods <- generateMethods ds
  return $ globals ++ methods

generateGlobals :: [Decl] -> CodeGen IRCode
-- Base case
generateGlobals [] = return []
-- A declaration without assignment
generateGlobals ((VarDecl typ var)       : ds) = do
  let vname = globalPrefix ++ var
  insertSymbol var vname
  return $ [IAlloc (AId vname)] ++ generateGlobals ds
-- A declaration with assignment
generateGlobals ((VAssignDecl typ var e) : ds) = do
  (lbl, code) <- generateExpr e
  let vname = globalPrefix ++ var
  insertSymbol var vname
  return $ [IAlloc (AId vname)] ++ code ++
    [ICopy vname lbl] ++ generateGlobals ds
-- An array declaration
generateGlobals ((VArrayDecl typ var e)  : ds) = do
  (lbl, code) <- generateExpr e
  let vname = globalPrefix ++ var
  insertSymbol var vname
  return $ code ++ [IAllocArr (AId vname) lbl] ++ generateGlobals ds
-- Other cases are dealt with in generateMethods
generateGlobals (d:ds) =
  generateGlobals ds


generateMethods :: [Decl] -> CodeGen IRCode
-- Base case
generateMethods [] = return []
-- Function declaration
generateMethods ((FuncDecl f ps _ body) : ds) = do
  uid <- uniqueNumber
  let fname = methodPrefix ++ f ++ "_" ++ uid
  insertSymbol f fname
  paramDecls <- generateFPs fname ps
  newSymbolTable
  bodyCode <- generateBody body
  removeSymbolTable
  return $
    [IMLabel fname] ++ paramDecls ++ bodyCode ++ [IExit] ++ generateMethods ds
-- Procedure declaration
generateMethods ((ProcDecl f ps body) : ds) = do
  uid <- uniqueNumber
  let fname = methodPrefix ++ f ++ "_" ++ uid
  insertSymbol f fname
  paramDecls <- generateFPs fname ps
  newSymbolTable
  bodyCode <- generateBody body
  return $
    [IMLabel fname] ++ paramDecls ++ bodyCode [IExit]++ generateMethods ds
-- Other cases are dealt with in generateGlobals
generateMethods (d:ds) = generateMethods ds

generateFPs :: String -> FormalParams -> CodeGen IRCode
generateFPs f (FPList ps) =
  mapM (generateFP f) (zip ps [0..])

generateFP :: String -> (FormalParam, Int) -> CodeGen Instr
generateFP f ((Param t var), ix) = do
  uid <- uniqueNumber
  let pname = paramPrefix ++ f ++ "_" ++ var ++ uid
  insertSymbol var pname
  return $ IAllocParam (AId pname) ix t

generateBody :: Body -> CodeGen IRCode
generateBody (DeclBody ds cst) = do
  dsCode <- generateDecls ds
  cstCode <- generateCompoundStmt cst
  return $ dsCode ++ cstCode
generateBody (StmtBody cst) =
  generateCompoundStmt cst
generateBody (EmptyBody) =
  return []

generateDecls :: Decls -> CodeGen IRCode
generateDecls (DeclList ds) =
  concat `liftM` (mapM generateDecl ds)

generateDecl :: Decl -> CodeGen IRCode
generateDecl (VarDecl typ var) = do
  uid <- uniqueNumber
  let vname = AId (localPrefix ++ var ++ "_" ++ uid)
  return [IAlloc vname]
-- A declaration with assignment
generateDecl (VAssignDecl typ var e) = do
  (lbl, code) <- generateExpr e
  uid <- uniqueNumber
  let vname = AId (localPrefix ++ var ++ "_" ++ uid)
  return $ [IAlloc vname] ++ code ++ [ICopy vname lbl]
-- An array declaration
generateDecl (VArrayDecl typ var e) = do
  (lbl, code) <- generateExpr e
  uid <- uniqueNumber
  let vname = AId (localPrefix ++ var ++ "_" ++ uid)
  return $ code ++ [AllocArr vname lbl]
-- A function declaration
generateDecl (FuncDecl f ps _ body) = do
  uid <- uniqueNumber
  let fname = AId (methodPrefix ++ f ++ "_" ++ uid)
  insertSymbol f fname
  paramDecls <- generateFPs f ps
  bodyCode <- generateBody body
  return $ [ILabel fname] ++ paramDecls ++ bodyCode
-- A procedure declaration
generateDecl (ProcDecl f ps body) = do
  uid <- uniqueNumber
  let fname = AId (methodPrefix ++ f ++ "_" ++ uid)
  insertSymbol f fname
  paramDecls <- generateFPs f ps
  bodyCode <- generateBody body
  return $ [ILabel fname] ++ paramDecls ++ bodyCode

generateExpr :: Expr -> CodeGen (Operand, IRCode)
generateExpr (EBinOp op e1 e2) = do
  (l1, e1c) <- generateExpr e1
  (l2, e2c) <- generateExpr e2
  rLbl <- uniqueLabel
  return (rLbl, e1c ++ e2c ++ [IAssignB rLbl l1 op l2])
generateExpr (EUnOp op e) = do
  (l, ec) <- generateExpr e
  rLbl <- uniqueLabel
  return (rLbl, ec ++ [IAssignU rLbl op l])
generateExpr (EId var) = do
  vname <- getDefinition var
  (AId vname, [])
generateExpr (EString str) = do
  return (AString rLbl str, [])
generateExpr (EInt i) = do
  return (AInt (fromInteger i), [])
generateExpr (EChar c) = do
  return (AChar c, [])
generateExpr (EArrRef arr e) = do
  (ix, code) <- generateExpr e
  aname <- getDefinition arr
  return (AArrRef (AId aname) ix, code)
generateExpr (EBkt e) =
  generateExpr e
generateExpr (ECall f aps@(APList as)) = do
  paramCode <- generateAPs aps
  rLbl <- uniqueLabel
  fname <- getDefinition f
  return (ACall (AId fname) (length as), paramCode)

generateAPs :: ActualParams -> CodeGen IRCode
generateAPs (APList aps) =
  concat `liftM` mapM generateAP aps

generateAP :: Expr -> CodeGen IRCode
generateAP e = do
  (lbl, code) <- generateExpr e
  return $ code ++ [IParam lbl]

generateStmt :: Stmt -> CodeGen IRCode
generateStmt (SBody b) =
  generateBody b
generateStmt (SNull) =
  []
generateStmt (SAssign e1 e2) = do
  (l1, code1) <- generateExpr e1
  (l2, code2) <- generateExpr e2
  return $ code1 ++ code2 ++ [ICopy l1 l2]
generateStmt (SInc e) = do
  (l, _) <- generateExpr e
  return [AssignB l l "+" (AInt 1)]
generateStmt (SDec e) = do
  (l, _) <- generateExpr e
  return [AssignB l l "-" (AInt 1)]
generateStmt (SReturn e) = do
  (l, code) = generateExpr e
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
  l1 <- uniqueLabel
  l2 <- uniqueLabel
  (cond, code) <- generateExpr expr
  loopBody <- generateCompoundStmt cst
  return $ [ILabel l1] ++ code ++ [ICGoto cond l2] ++
    loopBody ++ [IGoto l1] ++ [ILabel l2]
generateStmt (SIf ifs) = do
  endLbl <- uniqueLabel
  ifClauses <- generateIfClauses ifs endLbl
  return $ ifClauses ++ [ILabel endLbl]

generateIfClauses :: [IfClause] -> Label -> CodeGen IRCode
generateIfClauses ifs endlbl =
  concat `liftM` (mapM (generateIfClause endlbl) ifs)

generateIfClause :: IfClause -> Label -> CodeGen IRCode
generateIfClause (If cond cst) end = do
  (cl, code) <- generateExpr cond
  ifBody <- generateCompoundStmt cst
  endIf <- uniqueLabel
  return $ code ++ [INCGoto cl endIf] ++ ifBody ++ [IGoto end] ++ [ILabel endIf]
generateIfClause (Else cst) _ = do
  elseBody <- generateCompoundStmt cst
  return $ elseBody