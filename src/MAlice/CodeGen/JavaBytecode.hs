module MAlice.CodeGen.JavaBytecode where

import MAlice.CodeGen.JavaBytecodeInstr
import MAlice.CodeGen.JavaBytecodeOptimiser
import MAlice.CodeGen.JavaBytecodeUnitialisedReferences
import MAlice.CodeGen.JavaBytecodeStack
import MAlice.CodeGen.JavaBytecodeLocals
import MAlice.CodeGen.JavaBytecodeUtil
import MAlice.CodeGen.JavaBytecodeMissingReturns
import MAlice.CodeGen.JavaBytecodeThrowable
import MAlice.CodeGen.JavaBytecodeJunkLabels
import MAlice.CodeGen.JavaBytecodeSetupInput
import MAlice.CodeGen.JavaBytecodeOperators
import MAlice.Language.AST
import MAlice.Language.Types
import MAlice.SemanticAnalysis.ExprChecker
import Data.Char

translateProgram :: Program -> String -> JProgram
translateProgram (Program (DeclList decls)) thisClass
  = opt(
    [Class getClassName] ++
    [SuperClass]      ++
    setLocalVarNums ( 
      setupMethodStacks (
        convertConstructor (
          moveFieldsToTop(
            mergeConstructors(
              [MainMethod]      ++
              [Constructor []]  ++
              decls6
            )
          )
        )
      )
    )
    )
    where
      ! dothis   = setClassName thisClass
      (decls1, varTable, methTable, labelTable)
                 = translateGlobalDecls decls [] [] []
      junkLabels = getJunkLabels decls1
      decls2     = removeJunkLabels decls1 junkLabels
      decls3     = setupInputIfRequired decls2
      decls4     = setupMissingReturns decls3
      decls5     = setupThrowableIfRequired decls4
      (decls6, labelTable')
	         = setupUnitialisedReferences decls5 labelTable

translateGlobalDecls :: [Decl] -> VarTable -> MethTable -> LabelTable -> (JProgram, VarTable, MethTable, LabelTable)
translateGlobalDecls [] varTable methTable labelTable
  = ([], varTable, methTable, labelTable)
translateGlobalDecls (decl:rest) varTable methTable labelTable
  = (decl' ++ rest', varTable'', methTable'', labelTable'')
      where
        (decl', varTable', methTable', labelTable')    = translateGlobalDecl decl varTable methTable labelTable
        (rest', varTable'', methTable'', labelTable'') = translateGlobalDecls rest varTable' methTable' labelTable'

translateLocalDecls :: [Decl] -> VarTable -> MethTable -> LabelTable -> (JProgram, VarTable, MethTable, LabelTable)
translateLocalDecls [] varTable methTable labelTable
  = ([], varTable, methTable, labelTable)
translateLocalDecls (decl:rest) varTable methTable labelTable
  = (decl' ++ rest', varTable'', methTable'', labelTable'')
      where
        (decl', varTable', methTable', labelTable')    = translateLocalDecl decl varTable methTable labelTable
        (rest', varTable'', methTable'', labelTable'') = translateLocalDecls rest varTable' methTable' labelTable'


translateGlobalDecl :: Decl -> VarTable -> MethTable -> LabelTable -> (JProgram, VarTable, MethTable, LabelTable)
translateGlobalDecl (VarDecl t ident) varTable methTable labelTable
  = ([Field ident t'], ((Global ident t'):varTable), methTable, labelTable)
    where
      t' = translateToJType t
translateGlobalDecl (VAssignDecl (Sentence) ident expr) varTable methTable labelTable
  = ([Field ident t'] ++
     [(Constructor
       ([ALoad_0]                                                         ++
       [New "java/lang/String"]                                           ++
       [Dup]                                                              ++
       exprInstrs                                                          ++
       [Invokespecial "java/lang/String/<init>" "Ljava/lang/String;" "V"] ++
       [Putfield (getClassName++"/"++ident) t'])
      )],
     (Global ident t'):varTable, methTable, labelTable')
       where
         t'                        = translateToJType Sentence
         (exprInstrs, labelTable') = translateExpr expr varTable methTable labelTable
translateGlobalDecl (VAssignDecl t ident expr) varTable methTable labelTable
  = ([Field ident t'] ++
     [(Constructor
       ([ALoad_0]                              ++
       exprIntrs                               ++
       [Putfield (getClassName++"/"++ident) t'])
      )],
     (Global ident t'):varTable, methTable, labelTable')
       where
         t'                       = translateToJType t
         (exprIntrs, labelTable') = translateExpr expr varTable methTable labelTable
translateGlobalDecl (VArrayDecl t ident expr) varTable methTable labelTable
  = ([Field ident t''] ++
     [(Constructor
       ([ALoad_0]                              ++
       exprInstrs                              ++
       [Newarray t']                          ++
       [Putfield (getClassName++"/"++ident) t''])
      )],
     (Global ident t''):varTable, methTable, labelTable')
    where
      t'                        = translateToAType t
      t''                       = "[" ++translateToJType t
      (exprInstrs, labelTable') = translateExpr expr varTable methTable labelTable
translateGlobalDecl (FuncDecl ident (FPList formalParams) t body) varTable methTable labelTable
  = ([Func ident param returnString numParams] ++
     bodyInstrs                                ++
     [Endmethod],
     varTable, methTable', labelTable'')
       where
         methTable'                 = (Entry ident param returnString):methTable
         varTable'                  = moveParamsToLocals formalParams varTable 1
         numParams                  = getNumParams formalParams
         param                      = makeFormalParamTypeString formalParams
         returnString               = makeReturnString t
         (bodyInstrs, labelTable'') = translateBody body varTable' methTable' labelTable
translateGlobalDecl (ProcDecl ident (FPList formalParams) body) varTable methTable labelTable
  = ([Func ident param "V" numParams]           ++
     bodyInstrs                                 ++
     [Return]                                   ++
     [Endmethod],
     varTable, methTable', labelTable')
       where
         methTable'                = (Entry ident param "V"):methTable
         varTable'                 = moveParamsToLocals formalParams varTable 1
         numParams                 = getNumParams formalParams
         param                     = makeFormalParamTypeString formalParams
         (bodyInstrs, labelTable') = translateBody body varTable' methTable' labelTable

{- moveParamsToLocals :: [FormalParam] -> VarTable -> (JProgram, VarTable)
moveParamsToLocals [] varTable
  = ([], varTable)
moveParamsToLocals (param:rest) varTable
  = (param' ++ rest', varTable'')
    where
      (param', varTable') = moveParamToLocal param varTable
      (rest', varTable'') = moveParamsToLocals rest varTable'  -}

getNumParams :: [FormalParam] -> Int
getNumParams fp = length fp

moveParamsToLocals :: [FormalParam] -> VarTable -> Int -> VarTable
moveParamsToLocals [] varTable num
  = varTable
moveParamsToLocals (param:rest) varTable num
  = (param'++rest'++varTable)
    where
      (num', param') = moveParamToLocal param varTable num
      rest'          = moveParamsToLocals rest varTable num'

moveParamToLocal :: FormalParam -> VarTable -> Int -> (Int, VarTable)
moveParamToLocal (Param t ident) varTable num
  = (num+1, [Local ident num t'])
    where
      t' = translateToJType t

{- moveParamToLocal :: FormalParam -> VarTable -> (JProgram, VarTable)
moveParamToLocal (Param Number ident) varTable
  = ([IStore num], (Local ident num t'):varTable)
    where
      num = getNewLocalVar varTable 2
      t'  = translateToJType Number
moveParamToLocal (Param Sentence ident) varTable
  = ([AStore num], (Local ident num t'):varTable)
    where
      num = getNewLocalVar varTable 2
      t'  = translateToJType Sentence -}

translateLocalDecl  :: Decl -> VarTable -> MethTable -> LabelTable -> (JProgram, VarTable, MethTable, LabelTable)
translateLocalDecl (VarDecl t ident) varTable methTable labelTable
  = ([], (Local ident num t'):varTable, methTable, labelTable)
    where
      t'  = translateToJType t
      num = getNewLocalVar varTable 2
translateLocalDecl (VAssignDecl (Sentence) ident expr) varTable methTable labelTable
  = ([New "java/lang/String"]                                           ++
     [Dup]                                                              ++
     exprInstrs                                                         ++
     [Invokespecial "java/lang/String/<init>" "Ljava/lang/String;" "V"] ++
     [AStore num],
     (Local ident num t'):varTable, methTable, labelTable')
       where
         t'                        = translateToJType Sentence
         num                       = getNewLocalVar varTable 2
         (exprInstrs, labelTable') = translateExpr expr varTable methTable labelTable
translateLocalDecl (VAssignDecl t ident expr) varTable methTable labelTable
  = (exprInstrs ++
     [IStore num],
     (Local ident num t'):varTable, methTable, labelTable')
       where
         t'                        = translateToJType t
         num                       = getNewLocalVar varTable 2
         (exprInstrs, labelTable') = translateExpr expr varTable methTable labelTable
translateLocalDecl (VArrayDecl t ident expr) varTable methTable labelTable
  = (exprInstrs     ++
     [Newarray t''] ++
     [AStore num],
     (Local ident num t'):varTable, methTable, labelTable')
    where
      t'                        = "[" ++ translateToJType t
      t''                       = translateToAType t
      num                       = getNewLocalVar varTable 2
      (exprInstrs, labelTable') = translateExpr expr varTable methTable labelTable

translateBody :: Body -> VarTable -> MethTable -> LabelTable -> (JProgram, LabelTable)
translateBody (DeclBody (DeclList decls) (CSList stmts)) varTable methTable labelTable
  = (decls' ++ stmts', labelTable'')
    where
      (decls', varTable', methTable', labelTable')    = translateLocalDecls decls varTable methTable labelTable
      (stmts', varTable'', methTable'', labelTable'') = translateStmts stmts varTable' methTable' labelTable'
translateBody (StmtBody (CSList stmts)) varTable methTable labelTable
  = (stmts', labelTable')
     where
       (stmts', varTable', methTable', labelTable') = translateStmts stmts varTable methTable labelTable
translateBody EmptyBody varTable methTable labelTable
  = ([], labelTable)

translateStmts :: [Stmt] -> VarTable -> MethTable -> LabelTable -> (JProgram, VarTable, MethTable, LabelTable)
translateStmts [] varTable methTable labelTable
  = ([], varTable, methTable, labelTable)
translateStmts (stmt:rest) varTable methTable labelTable
  = (stmt' ++ rest', varTable'', methTable'', labelTable'')
     where
       (stmt', varTable', methTable', labelTable')    = translateStmt stmt varTable methTable labelTable
       (rest', varTable'', methTable'', labelTable'') = translateStmts rest varTable' methTable' labelTable'

translateStmt :: Stmt -> VarTable -> MethTable -> LabelTable -> (JProgram, VarTable, MethTable, LabelTable)
translateStmt (SBody body) varTable methTable labelTable
  = (body', varTable, methTable, labelTable')
     where
       (body', labelTable') = translateBody body varTable methTable labelTable
translateStmt SNull varTable methTable labelTable
  = ([], varTable, methTable, labelTable)
translateStmt (SAssign (EId t id) ex) varTable methTable labelTable
  = (exprInstrs ++
     translateVarAssign id varTable exprType,
     varTable, methTable, labelTable')
       where
         (exprInstrs, labelTable') = translateExpr ex varTable methTable labelTable
	 exprType                  = inferTypeP ex
translateStmt (SAssign (EArrRef t id ex1) ex2) varTable methTable labelTable
  = (translateVariable (lookupVarTableEntry id varTable) t  ++
     indexInstrs                                            ++
     exprInstrs                                             ++
     translateVarAssign id varTable exprType,
     varTable, methTable, labelTable'')
       where
         (indexInstrs, labelTable') = translateExpr ex1 varTable methTable labelTable
         (exprInstrs, labelTable'') = translateExpr ex2 varTable methTable labelTable'
	 exprType                   = inferTypeP ex2
translateStmt (SInc (EId t id)) varTable methTable labelTable
  = (translateVariable (lookupVarTableEntry id varTable) t ++
     [IConst_1]                                            ++
     [IAdd]                                                ++
     translateVarAssign id varTable Number,
     varTable, methTable, labelTable)
translateStmt (SInc (EArrRef t ident expr)) varTable methTable labelTable
  = (translateVariable (lookupVarTableEntry ident varTable) t ++
     [Dup]                                                    ++
     exprInstrs                                               ++
     [Dup]                                                    ++
     [IStore tempLoc]                                         ++
     [Swap]                                                   ++
     [ILoad tempLoc]                                          ++
     [IALoad]                                                 ++
     [IConst_1]                                               ++
     [IAdd]                                                   ++
     [IAStore],
     varTable, methTable, labelTable')
      where
        tempLoc                   = getNewLocalVar varTable 2
        (exprInstrs, labelTable') = translateExpr expr varTable methTable labelTable
translateStmt (SDec (EId t ident)) varTable methTable labelTable
  = (translateVariable (lookupVarTableEntry ident varTable) t ++
     [IConst_m1]                                              ++
     [IAdd]                                                   ++
     translateVarAssign ident varTable Number,
     varTable, methTable, labelTable)
translateStmt (SDec (EArrRef t ident expr)) varTable methTable labelTable
  = (translateVariable (lookupVarTableEntry ident varTable) t ++
     [Dup]                                                    ++
     exprInstrs                                               ++
     [Dup]                                                    ++
     [IStore tempLoc]                                         ++
     [Swap]                                                   ++
     [ILoad tempLoc]                                          ++
     [IALoad]                                                 ++
     [IConst_m1]                                              ++
     [IAdd]                                                   ++
     [IAStore],
     varTable, methTable, labelTable')
      where
        tempLoc                   = getNewLocalVar varTable 2
        (exprInstrs, labelTable') = translateExpr expr varTable methTable labelTable
translateStmt (SReturn ex) varTable methTable labelTable
  = (exprInstrs ++
    [returnInstr],
    varTable, methTable, labelTable')
        where
          (exprInstrs, labelTable') = translateExpr ex varTable methTable labelTable
          returnInstr               = translateReturnInstr ex
translateStmt (SPrint ex) varTable methTable labelTable
  = ([Getstatic "java/lang/System/out" "Ljava/io/PrintStream;"] ++
     exprInstrs                                                 ++
     charHandling                                               ++
     [Invokevirtual "java/io/PrintStream/print" t' "V"],
     varTable, methTable, labelTable')
  where
    t                         = inferTypeP ex
    t'                        = translateToJType t
    (exprInstrs, labelTable') = translateExpr ex varTable methTable labelTable
    charHandling              = printCharHandling t
translateStmt (SInput (EId t ident)) varTable methTable labelTable
  = ([ALoad_0]                                                ++
    [Getfield (getClassName++"/_scanner") "Ljava/util/Scanner;"] ++
    [Invokevirtual ("java/util/Scanner/"++scanMeth) "" t']    ++
    charHandling                                              ++
    translateVarAssign ident varTable t,
    varTable, methTable, labelTable)
      where
        t'           = translateToJTypeId t
        scanMeth     = getScanMeth t
        charHandling = inputCharHandling t
translateStmt (SInput (EArrRef t id expr)) varTable methTable labelTable
  = (translateVariable (lookupVarTableEntry id varTable) t    ++
    indexInstrs                                               ++
    [ALoad_0]                                                 ++
    [Getfield (getClassName++"/_scanner") "Ljava/util/Scanner;"] ++
    [Invokevirtual ("java/util/Scanner/"++scanMeth) "" t']    ++
    charHandling                                              ++
    translateVarAssign id varTable t,
    varTable, methTable, labelTable')
      where
        (indexInstrs, labelTable') = translateExpr expr varTable methTable labelTable
	t'                         = translateToJTypeId t
	scanMeth                   = getScanMeth t
	charHandling               = inputCharHandling t
translateStmt (SCall ident (APList exprs)) varTable methTable labelTable
  = ([ALoad_0]    ++
    paramsInstrs  ++
    [Invokevirtual callString paramString returnType] ++
    restoreRefs,
    varTable, methTable, labelTable')
      where
        (Entry ident' paramString returnType)
          = lookupMethTableEntry ident methTable
        callString
          = getClassName++"/"++ident'
        (paramsInstrs, labelTable', refTable)
          = translateParams exprs varTable methTable labelTable
	restoreRefs
	  = translateRestoreRefs varTable refTable
translateStmt (SLoop expr (CSList stmts)) varTable methTable labelTable
  = ([LLabel label] ++
    exprInstrs      ++
    [Ifne endLabel] ++
    stmtsInstrs     ++
    [Goto label]    ++
    [LLabel endLabel],
    varTable', methTable', labelTable'''')
      where
        (label, labelTable')        = generateNewLabel labelTable
        (endLabel, labelTable'')    = generateNewLabel labelTable'
        (exprInstrs, labelTable''') = translateExpr expr varTable methTable labelTable''
        (stmtsInstrs, varTable', methTable', labelTable'''')
                                    = translateStmts stmts varTable methTable labelTable'''
translateStmt (SIf clauses) varTable methTable labelTable
  = (instrs++[LLabel endLabel], varTable, methTable, labelTable'')
      where
        (endLabel, labelTable')
          = generateNewLabel labelTable
        (instrs, varTable', methTable', labelTable'')
          = translateClauses clauses varTable methTable labelTable' endLabel
translateStmt a b c d= error $ show a

translateReturnInstr :: Expr -> JInstr
translateReturnInstr ex
  = translateReturnInstr' (inferTypeP ex)
translateReturnInstr' :: Type -> JInstr
translateReturnInstr' Number
  = (IReturn)
translateReturnInstr' Letter
  = (IReturn)
translateReturnInstr' Sentence
  = (AReturn)

translateClauses :: [IfClause] -> VarTable -> MethTable -> LabelTable ->
                    String -> (JProgram, VarTable, MethTable, LabelTable)
translateClauses [] varTable methTable labelTable endLabel
  = ([], varTable, methTable, labelTable)
translateClauses (clause:rest) varTable methTable labelTable endLabel
  = (clauses'++rest', varTable'', methTable'', labelTable'')
      where
        (clauses', varTable', methTable', labelTable')
          = translateClause clause varTable methTable labelTable endLabel
        (rest', varTable'', methTable'', labelTable'')
          = translateClauses rest varTable' methTable' labelTable' endLabel

translateClause :: IfClause -> VarTable -> MethTable -> LabelTable -> String -> (JProgram, VarTable, MethTable, LabelTable)
translateClause (If expr (CSList stmts)) varTable methTable labelTable endLabel
  = (exprInstrs ++
    [Ifeq endIf] ++
    stmtsInstrs ++
    [Goto endLabel] ++
    [LLabel endIf],
    varTable', methTable', labelTable''')
    where
      (exprInstrs, labelTable')
        = translateExpr expr varTable methTable labelTable
      (endIf, labelTable'')
        = generateNewLabel labelTable'
      (stmtsInstrs, varTable', methTable', labelTable''')
        = translateStmts stmts varTable methTable labelTable''
translateClause (Else (CSList stmts)) varTable methTable labelTable endLabel
  = translateStmts stmts varTable methTable labelTable

translateVarAssign :: String -> VarTable -> Type -> JProgram
translateVarAssign ident varTable t
  = translateEntryAssign (lookupVarTableEntry ident varTable) varTable t
translateEntryAssign :: VarTableEntry -> VarTable -> Type -> JProgram
translateEntryAssign (Global ident t) varTable t'
  | t == "I"                  = standardGlobal (Global ident t)
  | t == "C"                  = standardGlobal (Global ident t)
  | t == "Ljava/lang/String;" = standardGlobal (Global ident t)
  | otherwise                 = arrayAccess (Global ident t) varTable
translateEntryAssign (Local ident loc "I") varTable t'
  = [(IStore loc)]
translateEntryAssign (Local ident loc "Ljava/lang/String;") varTable t'
  = [(AStore loc)]
translateEntryAssign (Local ident loc "C") varTable t'
  = [(IStore loc)]
translateEntryAssign (Local ident loc "Ljava/util/concurrent/atomic/AtomicReference;") varTable t'
  = translateLocalRefEntry (Local ident loc "") varTable t'
translateEntryAssign (Local ident loc arr) varTable t'
  = translateEntryAssignArr loc arr

translateLocalRefEntry :: VarTableEntry -> VarTable -> Type -> JProgram
translateLocalRefEntry (Local ident loc t) varTable Number
  = [IStore num]                                       ++
    [ALoad loc]                                        ++
    [New "java/lang/Integer"]                          ++
    [Dup]                                              ++
    [ILoad num]                                        ++
    [Invokespecial "java/lang/Integer/<init>" "I" "V"] ++
    [Invokevirtual "java/util/concurrent/atomic/AtomicReference.set" "Ljava/lang/Object;" "V"]
      where
        num = getNewLocalVar varTable 1
translateLocalRefEntry (Local ident loc t) varTable Letter
  = [IStore num]                                       ++
    [ALoad loc]                                        ++
    [New "java/lang/Integer"]                          ++
    [Dup]                                              ++
    [ILoad num]                                        ++
    [Invokespecial "java/lang/Integer/<init>" "I" "V"] ++
    [Invokevirtual "java/util/concurrent/atomic/AtomicReference.set" "Ljava/lang/Object;" "V"]
      where
        num = getNewLocalVar varTable 1
translateLocalRefEntry (Local ident loc t) varTable Sentence
  = [ALoad loc]  ++
    [Swap]       ++
    [Invokevirtual "java/util/concurrent/atomic/AtomicReference.set" "Ljava/lang/Object;" "V"]
      where
        num = getNewLocalVar varTable 1


standardGlobal :: VarTableEntry -> JProgram
standardGlobal (Global ident t)
  = [ALoad_0] ++
    [Swap]    ++
    [(Putfield (getClassName++"/"++ident) t)]
arrayAccess :: VarTableEntry -> VarTable -> JProgram
arrayAccess (Global ident t) varTable
  = {- [ALoad_0]                            ++
    [Getfield (thisClass++"/"++ident) t] ++
    [Swap]                               ++
    [IStore loc]                         ++
    [Swap]                               ++
    [ILoad loc]                          ++ -}
    arrayInstr
      where
        loc        = getNewLocalVar varTable 1
        arrayInstr = getArrayInstr t
getArrayInstr :: String -> JProgram
getArrayInstr "[I" = [IAStore]
getArrayInstr "[C" = [IAStore]
getArrayInstr  _   = [AAStore]


translateEntryAssignArr :: Int -> String -> JProgram
translateEntryAssignArr loc "[I"
  = [IAStore]
translateEntryAssignArr loc "[C"
  = [IAStore]
translateEntryAssignArr loc "[Ljava/lang/String;"
  = [AAStore]

translateExpr :: Expr -> VarTable -> MethTable -> LabelTable -> (JProgram, LabelTable)
translateExpr (EBinOp op ex1 ex2) varTable methTable labelTable
  = (ex1Instrs ++
    ex2Instrs ++
    binInstrs,
    labelTable''')
      where
        (ex1Instrs, labelTable')   = translateExpr ex1 varTable methTable labelTable
        (ex2Instrs, labelTable'')  = translateExpr ex2 varTable methTable labelTable'
        (binInstrs, labelTable''') = translateBinOp op labelTable''
translateExpr (EUnOp op ex) varTable methTable labelTable
  = (exInstrs ++
    unInstrs,
    labelTable')
      where
        (exInstrs, labelTable')  = translateExpr ex varTable methTable labelTable
        unInstrs                 = translateUnOp op
translateExpr (EId (Ref Number) ident) varTable methTable labelTable
  = ((translateVariable (lookupVarTableEntry ident varTable) Number)                          ++
    [Invokevirtual "java/util/concurrent/atomic/AtomicReference.get" "" "Ljava/lang/Object;"] ++
    [Checkcast "java/lang/Integer"]                                                           ++
    [Invokevirtual "java/lang/Integer.intValue" """I"], labelTable)
translateExpr (EId (Ref Letter) ident) varTable methTable labelTable
  = ((translateVariable (lookupVarTableEntry ident varTable) Letter)                          ++
    [Invokevirtual "java/util/concurrent/atomic/AtomicReference.get" "" "Ljava/lang/Object;"] ++
    [Checkcast "java/lang/Integer"]                                                           ++
    [Invokevirtual "java/lang/Integer.intValue" """I"], labelTable)

translateExpr (EId t ident) varTable methTable labelTable
  = (translateVariable (lookupVarTableEntry ident varTable) t, labelTable)
translateExpr (EString str) varTable methTable labelTable
  = ([(Ldc (ConsS str))], labelTable)
translateExpr (EInt int) varTable methTable labelTable
  = ([Ldc (ConsI int)], labelTable)
translateExpr (EChar char) varTable methTable labelTable
  = ([BIPush (ord char)], labelTable)
translateExpr (EArrRef t ident expr) varTable methTable labelTable
  = (translateVariable entry t  ++
    exInstrs                    ++
    [IALoad],
    labelTable')
      where
        (exInstrs, labelTable') = translateExpr expr varTable methTable labelTable
        entry                   = lookupVarTableEntry ident varTable
translateExpr (EBool b) varTable methTable labelTable
  | b         = ([BIPush 1], labelTable)
  | otherwise = ([BIPush 0], labelTable)
translateExpr (ECall t ident (APList exprs)) varTable methTable labelTable
  = ([ALoad_0]  ++
    paramsInstrs ++
    [Invokevirtual callString paramString returnType] ++
    restoreRefs,
    labelTable')
      where
        (paramsInstrs, labelTable', refTable) = translateParams exprs varTable methTable labelTable
        (Entry ident' paramString returnType) = lookupMethTableEntry ident methTable
        callString                            = getClassName++"/"++ident
	restoreRefs                           = translateRestoreRefs varTable refTable

translateRestoreRefs :: VarTable -> [(String, Int)] -> JProgram
translateRestoreRefs varTable []
  = []
translateRestoreRefs varTable (pair:pairs)
  = translateRestoreRef varTable pair ++
    translateRestoreRefs varTable pairs

translateRestoreRef :: VarTable -> (String, Int) -> JProgram
translateRestoreRef varTable (ident, num)
  = [ALoad num] ++
    [Invokevirtual "java/util/concurrent/atomic/AtomicReference.get" "" "Ljava/lang/Object;"] ++
    translateRestoreRefType (lookupVarTableEntry ident varTable)

translateRestoreRefType :: VarTableEntry -> JProgram
translateRestoreRefType (Global ident "I")
  = [Checkcast "java/lang/Integer"]                     ++
    [Invokevirtual "java/lang/Integer.intValue" "" "I"] ++
    [ALoad_0]                                           ++
    [Swap]                                              ++
    [Putfield field "I"]
      where
        field = getClassName++"/"++ident
translateRestoreRefType (Global ident "C")
  = [Checkcast "java/lang/Integer"]                     ++
    [Invokevirtual "java/lang/Integer.intValue" "" "I"] ++
    [ALoad_0]                                           ++
    [Swap]                                              ++
    [Putfield field "I"]
      where
        field = getClassName++"/"++ident
translateRestoreRefType (Global ident "Ljava/lang/String;")
  = [Checkcast "java/lang/String"] ++
    [ALoad_0]                      ++
    [Swap]                         ++
    [Putfield field t]
      where
        field = getClassName++"/"++ident
	t     = translateToJType Sentence
translateRestoreRefType (Local ident num "I")
  = [Checkcast "java/lang/Integer"]                     ++
    [Invokevirtual "java/lang/Integer.intValue" "" "I"] ++
    [IStore num]
translateRestoreRefType (Local ident num "C")
  = [Checkcast "java/lang/Integer"]                     ++
    [Invokevirtual "java/lang/Integer.intValue" "" "I"] ++
    [IStore num]
translateRestoreRefType (Local ident num "Ljava/lang/String;")
  = [Checkcast "java/lang/String"] ++
    [AStore num]

translateParams :: [Expr] -> VarTable -> MethTable -> LabelTable
                          -> (JProgram, LabelTable, [(String, Int)])
translateParams [] varTable methTable labelTable
  = ([], labelTable, [])
translateParams ((EId (Ref t) ident):rest) varTable methTable labelTable
  = ([NewAtomicReference]   ++
    [Dup]                   ++
    [Dup]                   ++
    makeNewVarObject        ++
    [InvokeAtomicReference] ++
    [AStore num]            ++
    params,
    labelTable', (ident, num):refTable)
    where
      (params, labelTable', refTable) = translateParams rest ((Local "." num "."):varTable) methTable labelTable
      makeNewVarObject                = translateObjectWrapper t ident varTable
      num                             = getNewLocalVar varTable 1
translateParams (expr:rest) varTable methTable labelTable
  = (exprInstrs ++ params, labelTable'', refTable)
      where
        (params, labelTable', refTable) = translateParams rest varTable methTable labelTable
        (exprInstrs, labelTable'')      = translateExpr expr varTable methTable labelTable'

translateObjectWrapper :: Type -> String -> VarTable -> JProgram
translateObjectWrapper Number ident varTable
  = [New "java/lang/Integer"] ++
    [Dup]                     ++
    identValueCode            ++
    [Invokespecial "java/lang/Integer/<init>" "I" "V"]
      where
        identValueCode = translateVariable (lookupVarTableEntry ident varTable) Number
translateObjectWrapper Letter ident varTable
  = [New "java/lang/Integer"] ++
    [Dup]                     ++
    identValueCode            ++
    [Invokespecial "java/lang/Integer/<init>" "I" "V"]
      where
        identValueCode = translateVariable (lookupVarTableEntry ident varTable) Number
translateObjectWrapper Sentence ident varTable
  = translateVariable (lookupVarTableEntry ident varTable) Sentence


translateVariable :: VarTableEntry -> Type -> JProgram
translateVariable (Global ident t) t'
  = [ALoad_0] ++
    [Getfield (getClassName++"/"++ident) t]
translateVariable (Local ident num "I") t
  = [ILoad num]
translateVariable (Local ident num "C") t
  = [ILoad num]
translateVariable (Local ident num "Ljava/lang/String;") t
  = [ALoad num]
translateVariable (Local ident num "Ljava/util/concurrent/atomic/AtomicReference;") t
  |t == Number || t == Letter
     = [ALoad num]                                                                               ++
       [Invokevirtual "java/util/concurrent/atomic/AtomicReference.get" "" "Ljava/lang/Object;"] ++
       [Checkcast "java/lang/Integer"]                                                           ++
       [Invokevirtual "java/lang/Integer.intValue" """I"]
  | t == Sentence
      = [ALoad num]                                                                               ++
        [Invokevirtual "java/util/concurrent/atomic/AtomicReference.get" "" "Ljava/lang/Object;"] ++
        [Checkcast "java/lang/String"] 
-- If not a primitive type load the reference onto the stack.
translateVariable (Local ident num t) t'
  = [ALoad num]

translateForParamString :: Type -> String
translateForParamString Number      = "I"
translateForParamString Letter      = "I"
translateForParamString Sentence    = "Ljava/lang/String;"
translateForParamString (RefType t) = "[" ++ translateForParamString t
translateForParamString (Ref t)     = "Ljava/util/concurrent/atomic/AtomicReference;"

translateToJType :: Type -> String
translateToJType Number       = "I"
translateToJType Letter       = "C"
translateToJType Sentence     = "Ljava/lang/String;"
translateToJType Boolean      = "C"
translateToJType (RefType t)  = "[" ++ translateToJType t
translateToJType (Ref t)      = "Ljava/util/concurrent/atomic/AtomicReference;" 
translateToJTypeId :: Type -> String
translateToJTypeId Number   = "I"
translateToJTypeId Letter   = "C"
translateToJTypeId Sentence = "Lhava/lang/String;"

translateToAType :: Type -> String
translateToAType Number = "int"
translateToAType Letter = "int"

getScanMeth :: Type -> String
getScanMeth Number   = "nextInt"
getScanMeth Letter   = "nextLine"
getScanMeth Sentence = "nextLine"

makeFormalParamTypeString :: [FormalParam] -> String
makeFormalParamTypeString []
  = ""
makeFormalParamTypeString ((Param t ident):rest)
  = translateForParamString t ++
    makeFormalParamTypeString rest

makeReturnString :: Type -> String
makeReturnString t
  = translateToJType t
