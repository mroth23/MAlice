module MAlice.CodeGen.JavaByteCode where

import MAlice.CodeGen.JavaByteCodeInstr
import MAlice.Language.AST
import MAlice.Language.Types
import MAlice.SemanticAnalysis.ExprChecker
import Data.Char

thisClass = "myclass"

translateProgram :: Program -> JProgram
translateProgram (Program (DeclList decls))
  = [Class thisClass] ++
    [SuperClass]      ++
    decls'
      where
        (decls', varTable, methTable) = translateGlobalDecls decls [] []

translateGlobalDecls :: [Decl] -> VarTable -> MethTable -> (JProgram, VarTable, MethTable)
translateGlobalDecls [] varTable methTable
  = ([], varTable, methTable)
translateGlobalDecls (decl:rest) varTable methTable
  = (decl' ++ rest', varTable'', methTable'')
      where 
        (decl', varTable', methTable')   = translateGlobalDecl decl varTable methTable
        (rest', varTable'', methTable'') = translateGlobalDecls rest varTable' methTable'

translateLocalDecls :: [Decl] -> VarTable -> MethTable -> (JProgram, VarTable, MethTable)
translateLocalDecls [] varTable methTable
  = ([], varTable, methTable)
translateLocalDecls (decl:rest) varTable methTable
  = (decl' ++ rest', varTable'', methTable'')
      where
        (decl', varTable', methTable')   = translateLocalDecl decl varTable methTable
	(rest', varTable'', methTable'') = translateLocalDecls rest varTable' methTable'
   

translateGlobalDecl :: Decl -> VarTable -> MethTable -> (JProgram, VarTable, MethTable)
translateGlobalDecl (VarDecl t ident) varTable methTable
  = ([Field ident t'], ((Global ident t'):varTable), methTable)
    where 
      t' = translateToJType t
translateGlobalDecl (VAssignDecl (Sentence) ident expr) varTable methTable
  = ([Field ident t'] ++
     [(Constructor 
       ([ALoad_0]                                                         ++
       [New "java/lang/String"]                                           ++
       [Dup]                                                              ++
       (translateExpr expr varTable methTable)                            ++
       [Invokespecial "java/lang/String/<init>" "Ljava/lang/String;" "V"] ++
       [Putfield (thisClass++"/"++ident) t'])
      )],
     (Global ident t'):varTable, methTable)
       where
         t' = translateToJType Sentence
translateGlobalDecl (VAssignDecl t ident expr) varTable methTable
  = ([Field ident t'] ++
     [(Constructor 
       ([ALoad_0]                              ++                              
       (translateExpr expr varTable methTable) ++
       [Putfield (thisClass++"/"++ident) t'])
      )], 
     (Global ident t'):varTable, methTable)
       where
         t' = translateToJType t
translateGlobalDecl (VArrayDecl t ident expr) varTable methTable
  = ([Field ident t'] ++
     [(Constructor
       ([ALoad_0]                              ++
       (translateExpr expr varTable methTable) ++
       [Newarray t'']                          ++
       [Putfield (thisClass++"/"++ident) t'])
      )],
     (Global ident t'):varTable, methTable)
    where
      t'  = translateToAType t
      t'' = translateToJType t
translateGlobalDecl (FuncDecl ident (FPList formalParams) t body) varTable methTable
  = ([Func ident param returnString]        ++
     parToLoc                               ++
     translateBody body varTable methTable' ++
     [Endmethod],
     varTable, methTable')
       where
         methTable'                         = (Entry ident param returnString):methTable
	 (parToLoc, varTable', methTable'') = moveParamsToLocals formalParams varTable methTable'
	 param                              = makeFormalParamTypeString formalParams
	 returnString                       = makeReturnString t
translateGlobalDecl (ProcDecl ident (FPList formalParams) body) varTable methTable
  = ([Func ident param "V"]                     ++
     translateBody body varTable methTable'     ++
     [Endmethod],
     varTable, methTable')
       where 
         methTable'   = (Entry ident param "V"):methTable
         param        = makeFormalParamTypeString formalParams

moveParamsToLocals :: [FormalParam] -> VarTable -> MethTable -> (JProgram, VarTable, MethTable)
moveParamsToLocals [] varTable methTable
  = ([], varTable, methTable)
moveParamsToLocals (param:rest) varTable methTable
  = (param' ++ rest', varTable'', methTable'')
    where
      (param', varTable', methTable')  = moveParamToLocal param varTable methTable
      (rest', varTable'', methTable'') = moveParamsToLocals rest varTable methTable  

moveParamToLocal :: FormalParam -> VarTable -> MethTable -> (JProgram, VarTable, MethTable)
moveParamToLocal (Param Number ident) varTable methTable
  = ([ILoad num], (Local ident num t'):varTable, methTable)
    where
      num = getNewLocalVar varTable 2 
      t'  = translateToJType Number
moveParamToLocal (Param Sentence ident) varTable methTable
  = ([ALoad num], (Local ident num t'):varTable, methTable)
    where
      num = getNewLocalVar varTable 2
      t'  = translateToJType Sentence

translateLocalDecl  :: Decl -> VarTable -> MethTable -> (JProgram, VarTable, MethTable)
translateLocalDecl (VarDecl t ident) varTable methTable
  = ([], (Local ident num t'):varTable, methTable)
    where
      t'  = translateToJType t
      num = getNewLocalVar varTable 2
translateLocalDecl (VAssignDecl (Sentence) ident expr) varTable methTable
  = (translateExpr expr varTable methTable ++
     [New "java/lang/String"] ++
     [Dup] ++
     translateExpr expr varTable methTable ++
     [Invokespecial "java/lang/String/<init>" "Ljava/lang/String;" "V"] ++
     [AStore num],
     (Local ident num t'):varTable, methTable)
       where
         t'  = translateToJType Sentence
	 num = getNewLocalVar varTable 2 
translateLocalDecl (VAssignDecl t ident expr) varTable methTable 
  = (translateExpr expr varTable methTable ++
     [IStore num],
     (Local ident num t'):varTable, methTable)
       where
         t'  = translateToJType t
         num = getNewLocalVar varTable 2
translateLocalDecl (VArrayDecl t ident expr) varTable methTable
  = ([ALoad_0]                             ++
     translateExpr expr varTable methTable ++
     [Newarray t'']                        ++
     [ALoad num],
     (Local ident num t'):varTable, methTable)
    where
      t'  = translateToJType t
      t'' = translateToAType t
      num = getNewLocalVar varTable 2

translateBody :: Body -> VarTable -> MethTable -> JProgram
translateBody (DeclBody (DeclList decls) (CSList stmts)) varTable methTable
  = decls' ++ stmts'
    where
      (decls', varTable', methTable')   = translateLocalDecls decls varTable methTable
      (stmts', varTable'', methTable'') = translateStmts stmts varTable' methTable'
translateBody (StmtBody (CSList stmts)) varTable methTable
  = stmts'
     where
       (stmts', varTable', methTable') = translateStmts stmts varTable methTable
translateBody EmptyBody varTable methTable
  = []

translateStmts :: [Stmt] -> VarTable -> MethTable -> (JProgram, VarTable, MethTable)
translateStmts [] varTable methTable
  = ([], varTable, methTable)
translateStmts (stmt:rest) varTable methTable
  = (stmt' ++ rest', varTable'', methTable'')
     where 
       (stmt', varTable', methTable')   = translateStmt stmt varTable methTable
       (rest', varTable'', methTable'') = translateStmts rest varTable' methTable'
  
translateStmt :: Stmt -> VarTable -> MethTable -> (JProgram, VarTable, MethTable)
translateStmt (SBody body) varTable methTable
  = (body', varTable, methTable)
     where 
       body' = translateBody body varTable methTable
translateStmt SNull varTable methTable
  = ([], varTable, methTable)
translateStmt (SAssign (EId t id) ex) varTable methTable
  = (translateExpr ex varTable methTable ++
     translateVarAssign id varTable,
     varTable, methTable)
translateStmt (SAssign (EArrRef t id ex1) ex2) varTable methTable
  = (translateVariable (lookupVarTableEntry id varTable) ++
     translateExpr ex1 varTable methTable                ++
     translateVarAssign id varTable,
     varTable, methTable)
translateStmt (SInc (EId t id)) varTable methTable
  = (translateVariable (lookupVarTableEntry id varTable) ++
     [IConst_1]                                          ++
     [IAdd]                                              ++
     translateVarAssign id varTable,
     varTable, methTable)
translateStmt (SInc (EArrRef t ident expr)) varTable methTable
  = (translateVariable (lookupVarTableEntry ident varTable) ++
     [Dup]                                                  ++
     translateExpr expr varTable methTable                  ++
     [Dup]                                                  ++
     [IStore tempLoc]                                       ++
     [Swap]                                                 ++
     [ILoad tempLoc]                                        ++
     [IALoad]                                               ++
     [IConst_1]                                             ++
     [IAdd]                                                 ++
     [IAStore],
     varTable, methTable)
      where
        tempLoc = getNewLocalVar varTable 2
translateStmt (SDec (EId t ident)) varTable methTable
  = (translateVariable (lookupVarTableEntry ident varTable) ++
     [IConst_m1]                                         ++
     [IAdd]                                              ++
     translateVarAssign ident varTable,
     varTable, methTable)
translateStmt (SDec (EArrRef t ident expr)) varTable methTable
  = (translateVariable (lookupVarTableEntry ident varTable) ++
     [Dup]                                                  ++
     translateExpr expr varTable methTable                  ++
     [Dup]                                                  ++
     [IStore tempLoc]                                       ++
     [Swap]                                                 ++
     [ILoad tempLoc]                                        ++
     [IALoad]                                               ++
     [IConst_m1]                                            ++
     [IAdd]                                                 ++
     [IAStore],
     varTable, methTable)
      where
        tempLoc = getNewLocalVar varTable 2
translateStmt (SReturn ex) varTable methTable
  = (translateExpr ex varTable methTable
      -- Insert return type, requires lookup
      , varTable, methTable)
translateStmt (SPrint ex) varTable methTable
  = ([Getstatic "java/lang/String/out" "Ljava/io/PrintStream;"] ++
     (translateExpr ex varTable methTable)                      ++
     [Invokevirtual "java/io/PrintStream/println" t' "V"], varTable, methTable)
  where
    (Just t) = inferTypeP ex
    t'       = translateToJType t 
translateStmt (SInput (EId t ident)) varTable methTable
  = undefined
translateStmt (SCall ident (APList exprs)) varTable methTable
  =  undefined

translateVarAssign :: String -> VarTable -> JProgram
translateVarAssign ident varTable
  = translateEntryAssign (lookupVarTableEntry ident varTable)
translateEntryAssign :: VarTableEntry -> JProgram
translateEntryAssign (Global ident t)
  = [ALoad_0] ++
    [Swap]    ++
    [(Putfield (thisClass++"/"++ident) t)]
translateEntryAssign (Local ident loc t)
  = [(IStore loc)]

translateExpr :: Expr -> VarTable -> MethTable -> JProgram
translateExpr (EBinOp op ex1 ex2) varTable methTable
  = (translateExpr ex1 varTable methTable) ++
    (translateExpr ex2 varTable methTable) ++
    (translateBinOp op)
translateExpr (EUnOp op ex) varTable methTable
  = (translateExpr ex varTable methTable) ++
    (translateUnOp op)
translateExpr (EId _ ident) varTable methTable
  = translateVariable (lookupVarTableEntry ident varTable)
translateExpr (EString str) varTable methTable
  = [(Ldc (ConsS str))]
translateExpr (EInt int) varTable methTable
  = [ILoad int]
translateExpr (EChar char) varTable methTable
  = [BIPush (ord char)]
translateExpr (EArrRef t ident expr) varTable methTable
  = translateVariable (lookupVarTableEntry ident varTable) ++
    translateExpr expr varTable methTable                  ++
    [IALoad]
translateExpr (EBool b) varTable methTable
  | b         = [BIPush 1]
  | otherwise = [BIPush 0]
translateExpr (ECall t ident (APList exprs)) varTable methTable
  = [ALoad_0]                                ++
    translateParams exprs varTable methTable ++
    [Invokevirtual callString paramString returnType]
      where
        (Entry ident' paramString returnType) = lookupMethTableEntry ident methTable
	callString                            = thisClass++"/"++ident'

translateParams :: [Expr] -> VarTable -> MethTable -> JProgram
translateParams [] varTable methTable
  = []
translateParams (expr:rest) varTable methTable
  = translateParams rest varTable methTable ++
    translateExpr expr varTable methTable

translateVariable :: VarTableEntry -> JProgram
translateVariable (Global ident t)
  = [ALoad_0] ++
    [Getfield (thisClass++"/"++ident) t]
translateVariable (Local ident num "I")
  = [ILoad num]
-- Need to convert chars to ints.
translateVariable (Local ident num "C")
  = [ILoad num]
translateVariable (Local ident num "Ljava/lang/String;")
  = [ALoad num]
-- If not a primitive type load the reference onto the stack.
translateVariable (Local ident num t)
  = [ALoad num]

translateBinOp :: String -> JProgram
translateBinOp "+" = [IAdd]
translateBinOp "-" = [ISub]
translateBinOp "*" = [IMul]
translateBinOp "/" = [IDiv]

translateUnOp :: String -> JProgram
translateUnOp
  = undefined

translateToJType :: Type -> String
translateToJType Number
  = "I"
translateToJType Letter
  = "C"
translateToJType Sentence
  = "Ljava/lang/String;"
translateToJType Boolean
  = "C"
translateToJType (RefType t)
  = "[" ++ translateToJType t

translateToAType :: Type -> String
translateToAType Number
  = "int"

type VarTable = [VarTableEntry]

data VarTableEntry
  --       Ident  Type           Ident  Loc Type
  = Global String String | Local String Int String
    deriving (Show, Eq)
lookupVarTableEntry :: String -> VarTable -> VarTableEntry
lookupVarTableEntry str ((Global ident t):rest)
  | str == ident = (Global ident t)
lookupVarTableEntry str ((Local ident int t):rest)
  | str == ident = (Local ident int t)
lookupVarTableEntry str (entry:rest)
  = lookupVarTableEntry str rest

type MethTable = [MethTableEntry]

data MethTableEntry
    --    Ident  Param  Return
  = Entry String String String

lookupMethTableEntry :: String -> MethTable -> MethTableEntry
lookupMethTableEntry str ((Entry ident param ret):rest)
  | str == ident = (Entry ident param ret)
  | otherwise    = lookupMethTableEntry str rest

makeFormalParamTypeString :: [FormalParam] -> String
makeFormalParamTypeString []
  = ""
makeFormalParamTypeString ((Param t ident):rest)
  = translateToJType t ++
    makeFormalParamTypeString rest

makeReturnString :: Type -> String
makeReturnString t
  = translateToJType t

getNewLocalVar :: VarTable -> Int -> Int
getNewLocalVar table num
  | tryLocalVar table num = num
  | otherwise             = getNewLocalVar table (num+1)
tryLocalVar :: VarTable -> Int -> Bool
tryLocalVar [] num
  = True
tryLocalVar ((Local ident int t):rest) num
  | num == int = False
tryLocalVar (_:rest) num
  = tryLocalVar rest num

showJavaProgram :: JProgram -> IO ()
showJavaProgram program = mapM_ print program
