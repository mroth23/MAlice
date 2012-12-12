module MAlice.CodeGen.ExpressionAnalysis where

import Data.List
import MAlice.Language.AST
import MAlice.Language.Types

-- Remove variable declerations and assignments if variable isn't used.
wasteRemover :: Program -> Program
wasteRemover (Program (DeclList decls))
 = removeWaste (Program (DeclList decls')) (generateTopLevelIdentTable decls)
   where
     decls' = wasteRemover'' decls

wasteRemover'' :: [Decl] -> [Decl]
wasteRemover'' []
  = []
wasteRemover'' ((VarDecl t i):rest)
  = [VarDecl t i] ++ wasteRemover'' rest
wasteRemover'' ((VAssignDecl t i e):rest)
  = [VAssignDecl t i e] ++ wasteRemover'' rest
wasteRemover'' ((VArrayDecl t i e):rest)
  = [VArrayDecl t i e] ++ wasteRemover'' rest
wasteRemover'' ((FuncDecl i fp t (DeclBody (DeclList decls) comStmt)):rest)
  = [FuncDecl i fp t (DeclBody (DeclList decls') comStmt)] ++ wasteRemover'' rest
    where
      decls'  = removeWaste' decls'' (generateTopLevelIdentTable decls'')
      decls'' = wasteRemover'' decls
wasteRemover'' ((FuncDecl i fp t (StmtBody comStmt)):rest)
  = [FuncDecl i fp t (StmtBody comStmt)] ++ wasteRemover'' rest
wasteRemover'' ((ProcDecl i fp (DeclBody (DeclList decls) comStmt)):rest)
  = [ProcDecl i fp (DeclBody (DeclList decls') comStmt)] ++ wasteRemover'' rest
    where
      decls'  = removeWaste' decls'' (generateTopLevelIdentTable decls'')
      decls'' = wasteRemover'' decls
wasteRemover'' ((ProcDecl i fp (StmtBody comStmt)):rest)
  = [ProcDecl i fp (StmtBody comStmt)] ++ wasteRemover'' rest

removeWaste :: Program -> IdentTable -> Program
removeWaste (Program (DeclList decls)) table
  = (Program (DeclList (removeWasteInDecls decls table)))


removeWaste' :: [Decl] -> IdentTable -> [Decl]
removeWaste' decls table
  = (removeWasteInDecls decls table)

removeWasteInDecls :: [Decl] -> IdentTable -> [Decl]
removeWasteInDecls decls table
  = removeGivenDecals decls (produceTableToRemove decls table)

produceTableToRemove :: [Decl] -> IdentTable -> IdentTable
produceTableToRemove decls []
  = []
produceTableToRemove decls (x:xs)
  | identUses x decls >= 2 = produceTableToRemove decls xs
  | otherwise              = [x] ++ produceTableToRemove decls xs

identUses :: Ident -> [Decl] -> Int
identUses ident []
  = 0
identUses ident (x:xs)
  = (useDecl ident x) + identUses ident xs

useDecl :: Ident -> Decl -> Int
useDecl key (VarDecl t ident)
  | key == ident = 1
useDecl key (VAssignDecl t ident expr)
  | key == ident = 1
useDecl key (VArrayDecl t ident expr)
  | key == ident = 1
useDecl key (FuncDecl ident fp t body)
  | key == ident = 1 + useFunction key (FuncDecl ident fp t body)
  | otherwise    = useFunction key (FuncDecl ident fp t body)
useDecl key (ProcDecl ident fp body)
  | key == ident = 1 + useProcedure key (ProcDecl ident fp body)
  | otherwise    = useProcedure key (ProcDecl ident fp body)
useDecl _ _
  = 0

useFunction :: Ident -> Decl -> Int
useFunction key (FuncDecl ident (FPList fp) t body)
  = if (inParam key fp)
      then
        0
      else
        useBody key body
useFunction _ _
  = 0

useProcedure :: Ident -> Decl -> Int
useProcedure key (ProcDecl ident (FPList fp) body)
  = if (inParam key fp)
      then
        0
      else
        useBody key body
useProcedure _ _
  = 0

-- Check if variables are redeclared in the formal parameters.
inParam :: Ident -> [FormalParam] -> Bool
inParam key []
  = False
inParam key ((Param t ident):rest)
  | key == ident = True
  | otherwise    = inParam key rest

useBody :: Ident -> Body -> Int
useBody key (DeclBody (DeclList decls) comStmt)
  = identUses key decls + useComStmt key comStmt
useBody key (StmtBody comStmt)
  = useComStmt key comStmt
useBody key EmptyBody
  = 0

useComStmt :: Ident -> CompoundStmt -> Int
useComStmt key (CSList stmtList)
  = useComStmt' key stmtList

useComStmt' :: Ident -> [Stmt] -> Int
useComStmt' key []
  = 0
useComStmt' key (x:xs)
  = useStmt key x + useComStmt' key xs

useStmt :: Ident -> Stmt -> Int
useStmt key (SBody body)
  = useBody key body
useStmt key (SNull)
  = 0
useStmt key (SAssign expr1 expr2)
  = useExpr key expr1 + useExpr key expr2
useStmt key (SInc expr)
  = useExpr key expr
useStmt key (SDec expr)
  = useExpr key expr
useStmt key (SReturn expr)
  = useExpr key expr
useStmt key (SPrint expr)
  = useExpr key expr
useStmt key (SInput expr)
  = useExpr key expr
useStmt key (SCall ident (APList exprs))
  | key == ident = 1 + useExprList key exprs
  | otherwise    = useExprList key exprs
useStmt key (SLoop expr comStmt)
  = useExpr key expr + useComStmt key comStmt
{- useStmt key (SIf exprComList)
  = useExprComList key exprComList -}

useExprComList :: Ident -> [(Expr, CompoundStmt)] -> Int
useExprComList key []
  = 0
useExprComList key ((expr, comStmt):rest)
  = useExpr key expr + useComStmt key comStmt

useExprList :: Ident -> [Expr] -> Int
useExprList key []
  = 0
useExprList key (expr:rest)
  = useExpr key expr + useExprList key rest

useExpr :: Ident -> Expr -> Int
useExpr key (EPlus ex1 ex2)
  = useExpr key ex1 + useExpr key ex2
useExpr key (EMinus ex1 ex2)
  = useExpr key ex1 + useExpr key ex2
useExpr key (EMult ex1 ex2)
  = useExpr key ex1 + useExpr key ex2
useExpr key (EDiv ex1 ex2)
  = useExpr key ex1 + useExpr key ex2
useExpr key (EMod ex1 ex2)
  = useExpr key ex1 + useExpr key ex2
useExpr key (EBAnd ex1 ex2)
  = useExpr key ex1 + useExpr key ex2
useExpr key (EBOr ex1 ex2)
  = useExpr key ex1 + useExpr key ex2
useExpr key (EBXor ex1 ex2)
  = useExpr key ex1 + useExpr key ex2
useExpr key (ELOr ex1 ex2)
  = useExpr key ex1 + useExpr key ex2
useExpr key (ELAnd ex1 ex2)
  = useExpr key ex1 + useExpr key ex2
useExpr key (EGT ex1 ex2)
  = useExpr key ex1 + useExpr key ex2
useExpr key (EGTE ex1 ex2)
  = useExpr key ex1 + useExpr key ex2
useExpr key (EEq ex1 ex2)
  = useExpr key ex1 + useExpr key ex2
useExpr key (ELTE ex1 ex2)
  = useExpr key ex1 + useExpr key ex2
useExpr key (ELT ex1 ex2)
  = useExpr key ex1 + useExpr key ex2
useExpr key (ENEq ex1 ex2)
  = useExpr key ex1 + useExpr key ex2
useExpr key (ENot ex)
  = useExpr key ex
useExpr key (EInv ex)
  = useExpr key ex
useExpr key (EId ident)
  | key == ident = 1
  | otherwise    = 0
useExpr key (EString str)
  = 0
useExpr key (EChar char)
  = 0
useExpr key (EArrRef ident ex)
  = useExpr key (EId ident) + useExpr key ex
useExpr key (EBkt ex)
  = useExpr key ex
useExpr key (ECall ident (APList exprs))
  = useExpr key (EId ident) + useExprList key exprs
useExpr key (ENegate ex)
  = useExpr key ex
useExpr key (EPositive ex)
  = useExpr key ex
useExpr _ _
  = 0

removeGivenDecals :: [Decl] -> IdentTable -> [Decl]
removeGivenDecals decls []
  = []
removeGivenDecals decls (x:xs)
  = removeGivenDecals decls' xs
    where
      decls' = removeDecl decls x

removeDecl :: [Decl] -> Ident -> [Decl]
removeDecl [] key
  = []
removeDecl ((VarDecl t ident):xs) key
  | ident == key = xs
removeDecl ((VAssignDecl t ident expr):xs) key
  | ident == key = xs
removeDecl ((VArrayDecl t ident expr):xs) key
  | ident == key = xs
removeDecl ((FuncDecl ident fp t body):xs) key
  | ident == key = xs
removeDecl ((ProcDecl ident fp body):xs) key
  | ident == key = xs
removeDecl (x:xs) key
  = [x] ++ removeDecl xs key

generateTopLevelIdentTable :: [Decl] -> IdentTable
generateTopLevelIdentTable decls
  = generateTopLevelIdentTable' decls []
generateTopLevelIdentTable' :: [Decl] -> IdentTable -> IdentTable
generateTopLevelIdentTable' [] table
  = table
generateTopLevelIdentTable' ((VarDecl t ident):rest) table
  = generateTopLevelIdentTable' rest table'
    where
      table' = addToTable ident table
generateTopLevelIdentTable' ((VAssignDecl t ident expr):rest) table
  = generateTopLevelIdentTable' rest table'
    where
      table' = addToTable ident table
generateTopLevelIdentTable' ((VArrayDecl t ident expr):rest) table
  = generateTopLevelIdentTable' rest table'
    where
      table' = addToTable ident table
generateTopLevelIdentTable' ((FuncDecl ident fp t body):rest) table
  = generateTopLevelIdentTable' rest table'
    where
      table' = addToTable ident table
generateTopLevelIdentTable' ((ProcDecl ident fp body):rest) table
  = generateTopLevelIdentTable' rest table'
    where
      table' = addToTable ident table

type IdentTable = [Ident]

tableContains :: Ident -> IdentTable -> Bool
tableContains _ []
  = False
tableContains key (curr:rest)
  | key == curr = True
  | otherwise   = tableContains key rest

addToTable :: Ident -> IdentTable -> IdentTable
addToTable ident table
  = if (tableContains ident table)
    then
      table
    else
      table ++ [ident]

-- Use variable table to generate new variable names.
generateVarTable :: Program -> IdentTable
generateVarTable (Program (DeclList decls))
  = nub (generateVarTableDecls decls)

generateVarTableDecls :: [Decl] -> IdentTable
generateVarTableDecls []
  = []
generateVarTableDecls (decl:rest)
  = generateVarTableDecl decl ++ generateVarTableDecls rest

generateVarTableDecl :: Decl -> IdentTable
generateVarTableDecl (VarDecl t ident)
  = [ident]
generateVarTableDecl (VAssignDecl t ident e)
  = [ident]
generateVarTableDecl (VArrayDecl t ident e)
  = [ident]
generateVarTableDecl (FuncDecl ident (FPList list) t body)
  = [ident] ++ generateVarTableFormalParam list ++ generateVarTableBody body
generateVarTableDecl (ProcDecl ident (FPList list) body)
  = [ident] ++ generateVarTableFormalParam list ++ generateVarTableBody body

generateVarTableFormalParam :: [FormalParam] -> IdentTable
generateVarTableFormalParam []
  = []
generateVarTableFormalParam ((Param t ident):rest)
  = [ident] ++ generateVarTableFormalParam rest

generateVarTableBody :: Body -> IdentTable
generateVarTableBody EmptyBody
  = []
generateVarTableBody (DeclBody (DeclList decls) (CSList stmts))
  = generateVarTableDecls decls ++ generateVarTableStmts stmts
generateVarTableBody (StmtBody (CSList stmts))
  = generateVarTableStmts stmts

generateVarTableStmts :: [Stmt] -> IdentTable
generateVarTableStmts []
  = []
generateVarTableStmts (stmt:rest)
  = generateVarTableStmt stmt ++ generateVarTableStmts rest

generateVarTableStmt :: Stmt -> IdentTable
generateVarTableStmt (SBody body)
  = generateVarTableBody body
generateVarTableStmt SNull
  = []
generateVarTableStmt (SAssign ex1 ex2)
  = generateVarTableExpr ex1 ++ generateVarTableExpr ex2
generateVarTableStmt (SInc ex)
  = generateVarTableExpr ex
generateVarTableStmt (SDec ex)
  = generateVarTableExpr ex
generateVarTableStmt (SReturn ex)
  = generateVarTableExpr ex
generateVarTableStmt (SPrint ex)
  = generateVarTableExpr ex
generateVarTableStmt (SInput ex)
  = generateVarTableExpr ex
generateVarTableStmt (SCall ident (APList exprs))
  = [ident] ++ generateVarTableExprs exprs
generateVarTableStmt (SLoop expr (CSList stmts))
  = generateVarTableExpr expr ++ generateVarTableStmts stmts
generateVarTableStmt (SIf ifList)
  = generateVarTableIf ifList

generateVarTableIf :: [IfClause] -> IdentTable
generateVarTableIf []
  = []
generateVarTableIf ((If ex (CSList stmts)):rest)
  = generateVarTableExpr ex ++ generateVarTableStmts stmts ++ generateVarTableIf rest
generateVarTableIf ((Else (CSList stmts)):rest)
  = generateVarTableStmts stmts ++ generateVarTableIf rest

generateVarTableExprs :: [Expr] -> IdentTable
generateVarTableExprs []
  = []
generateVarTableExprs (expr:rest)
  = generateVarTableExpr expr ++ generateVarTableExprs rest

generateVarTableExpr :: Expr -> IdentTable
generateVarTableExpr (EPlus ex1 ex2)
  = generateVarTableExpr ex1 ++ generateVarTableExpr ex2
generateVarTableExpr (EMinus ex1 ex2)
  = generateVarTableExpr ex1 ++ generateVarTableExpr ex2
generateVarTableExpr (EMult ex1 ex2)
  = generateVarTableExpr ex1 ++ generateVarTableExpr ex2
generateVarTableExpr (EDiv ex1 ex2)
  = generateVarTableExpr ex1 ++ generateVarTableExpr ex2
generateVarTableExpr (EMod ex1 ex2)
  = generateVarTableExpr ex1 ++ generateVarTableExpr ex2
generateVarTableExpr (EBAnd ex1 ex2)
  = generateVarTableExpr ex1 ++ generateVarTableExpr ex2
generateVarTableExpr (EBOr ex1 ex2)
  = generateVarTableExpr ex1 ++ generateVarTableExpr ex2
generateVarTableExpr (EBXor ex1 ex2)
  = generateVarTableExpr ex1 ++ generateVarTableExpr ex2
generateVarTableExpr (ELOr ex1 ex2)
  = generateVarTableExpr ex1 ++ generateVarTableExpr ex2
generateVarTableExpr (ELAnd ex1 ex2)
  = generateVarTableExpr ex1 ++ generateVarTableExpr ex2
generateVarTableExpr (EGT ex1 ex2)
  = generateVarTableExpr ex1 ++ generateVarTableExpr ex2
generateVarTableExpr (EGTE ex1 ex2)
  = generateVarTableExpr ex1 ++ generateVarTableExpr ex2
generateVarTableExpr (EEq ex1 ex2)
  = generateVarTableExpr ex1 ++ generateVarTableExpr ex2
generateVarTableExpr (ELTE ex1 ex2)
  = generateVarTableExpr ex1 ++ generateVarTableExpr ex2
generateVarTableExpr (ELT ex1 ex2)
  = generateVarTableExpr ex1 ++ generateVarTableExpr ex2
generateVarTableExpr (ENEq ex1 ex2)
  = generateVarTableExpr ex1 ++ generateVarTableExpr ex2
generateVarTableExpr (ENot ex)
  = generateVarTableExpr ex
generateVarTableExpr (EInv ex)
  = generateVarTableExpr ex
generateVarTableExpr (EId ident)
  = [ident]
generateVarTableExpr (EString _)
  = []
generateVarTableExpr (EInt _)
  = []
generateVarTableExpr (EChar _)
  = []
generateVarTableExpr (EArrRef ident expr)
  = [ident] ++ generateVarTableExpr expr
generateVarTableExpr (EBkt ex)
  = generateVarTableExpr ex
generateVarTableExpr (ECall ident (APList exprs))
  = [ident] ++ generateVarTableExprs exprs
generateVarTableExpr (ENegate ex)
  = generateVarTableExpr ex
generateVarTableExpr (EPositive ex)
  = generateVarTableExpr ex

generateNewVar :: IdentTable -> Ident
generateNewVar []
  = "temp1"
generateNewVar table
  = var
    where
      (var, int) = generateNewVar' ("temp1", 1) table

generateNewVar' :: (Ident, Int) -> IdentTable -> (Ident, Int)
generateNewVar' (var, int) table
  | elem var table = generateNewVar' (generateNextVar (var, int)) table
  | otherwise      = (var, int)

generateNextVar :: (Ident, Int) -> (Ident, Int)
generateNextVar (ident, int)
  = (ident', int')
    where
      ident' = "temp" ++ (show int')
      int'   = int + 1

splitExpressions :: Program -> Program
splitExpressions (Program (DeclList decls))
  = Program (DeclList decls')
    where
      decls' = splitExDecls decls

splitExDecls :: [Decl] -> [Decl]
splitExDecls []
  = []
splitExDecls (decl:rest)
  = splitExDecl decl ++ splitExDecls rest

splitExDecl :: Decl -> [Decl]
splitExDecl (VarDecl t i)
  = (VarDecl t i)
splitExDecl (VAssignDecl t i ex)
  = exprs ++ [VAssignDecl t i ex']
    where
      (exprs, ex') = splitExDecl' ex
splitExDecl (VArrayDecl t i ex)
  = exprs ++ [VAssignDecl t i ex']
    where
      (exprs, ex') = splitExDecl' ex
splitExDecl (FuncDecl i fp t body)
  = [FuncDecl i fp t body']
    where body' = splitExBody body
splitExDecl (ProcDecl i fp body)
  = [ProcDecl i fp body']
    where
      body' = splitExBody body

splitExDecl' :: Expr -> ([Decl], ex)
splitExDecl' (

splitExBody :: Body -> Body
splitExBody (DeclBody (DeclList decls) (CSList stmts))
  = (DeclBody (DeclList decls') (CSList stmts'))
    where
      decls' = splitExDecls decls
      stmts' = splitExStmts stmts
splitExBody (StmtBody (CSList stmts))
  = (StmtBody (CSList stmts'))
    where
      stmts' = splitExStmts stmts
splitExBody EmptyBody
  = EmptyBody

splitExStmts :: [Stmt] -> [Stmt]
splitExStmts []
  = []
splitExStmts (stmt:rest)
  = [splitExStmt stmt] ++ splitExStmts rest

splitExStmt :: Stmt -> Stmt
splitExStmt (SBody body)
  = (SBody body')
    where
      body' = splitExBody body
splitExStmt SNull
  = SNull
splitExStmt (SAssign ex1 ex1)
  = (SBody (DeclBody exprs) (CSList [(SAssign ex1' ex2')]))
    where
      (exprs, ex1', ex2') = splitEx ex1 ex2
splitExStmt (SInc ex)
  = (SInc ex')
    where
      ex' = splitEx ex
splitExStmt (SDec ex)
  = (SDec ex')
    where
      ex' = splitEx ex
splitExStmt (SReturn ex)
  = (SReturn ex')
    where
      ex' = splitEx ex
splitExStmt (SPrint ex)
  = (SPrint ex')
    where
      ex' =
