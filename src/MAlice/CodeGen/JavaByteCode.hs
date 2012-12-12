module MAlice.CodeGen.JavaByteCode where

import MAlice.CodeGen.JavaByteCodeInstr
import MAlice.CodeGen.JavaByteCodeOptimiser
import MAlice.Language.AST
import MAlice.Language.Types
import MAlice.SemanticAnalysis.ExprChecker
import Data.Char

thisClass = "Myclass"

translateProgram :: Program -> JProgram
translateProgram (Program (DeclList decls))
  = opt(
    [Class thisClass] ++
    [SuperClass]      ++
    moveFieldsToTop(
      mergeConstructors(
        [MainMethod]      ++
        [Constructor []]  ++
        decls'''
      )
    )
    )
      where
        (decls', varTable, methTable, labelTable)
                   = translateGlobalDecls decls [] [] []
        junkLabels = getJunkLabels decls'
        decls''    = removeJunkLabels decls' junkLabels
        decls'''   = setupInputIfRequired decls''

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
       [Putfield (thisClass++"/"++ident) t'])
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
       [Putfield (thisClass++"/"++ident) t'])
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
       [Putfield (thisClass++"/"++ident) t''])
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
getNumParams []
  = 0
getNumParams (param:rest)
  = (getNumParams rest) + 1

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
  = (body', varTable, methTable, labelTable)
     where
       (body', labelTable') = translateBody body varTable methTable labelTable
translateStmt SNull varTable methTable labelTable
  = ([], varTable, methTable, labelTable)
translateStmt (SAssign (EId t id) ex) varTable methTable labelTable
  = (exprInstrs ++
     translateVarAssign id varTable,
     varTable, methTable, labelTable')
       where
         (exprInstrs, labelTable') = translateExpr ex varTable methTable labelTable
-- NOT COMPLETED YET --
translateStmt (SAssign (EArrRef t id ex1) ex2) varTable methTable labelTable
  = (translateVariable (lookupVarTableEntry id varTable) ++
     indexInstrs                                         ++
     exprInstrs                                          ++
     translateVarAssign id varTable,
     varTable, methTable, labelTable'')
       where
         (indexInstrs, labelTable') = translateExpr ex1 varTable methTable labelTable
         (exprInstrs, labelTable'') = translateExpr ex2 varTable methTable labelTable'
translateStmt (SInc (EId t id)) varTable methTable labelTable
  = (translateVariable (lookupVarTableEntry id varTable) ++
     [IConst_1]                                          ++
     [IAdd]                                              ++
     translateVarAssign id varTable,
     varTable, methTable, labelTable)
translateStmt (SInc (EArrRef t ident expr)) varTable methTable labelTable
  = (translateVariable (lookupVarTableEntry ident varTable) ++
     [Dup]                                                  ++
     exprInstrs                                             ++
     [Dup]                                                  ++
     [IStore tempLoc]                                       ++
     [Swap]                                                 ++
     [ILoad tempLoc]                                        ++
     [IALoad]                                               ++
     [IConst_1]                                             ++
     [IAdd]                                                 ++
     [IAStore],
     varTable, methTable, labelTable')
      where
        tempLoc                   = getNewLocalVar varTable 2
        (exprInstrs, labelTable') = translateExpr expr varTable methTable labelTable
translateStmt (SDec (EId t ident)) varTable methTable labelTable
  = (translateVariable (lookupVarTableEntry ident varTable) ++
     [IConst_m1]                                         ++
     [IAdd]                                              ++
     translateVarAssign ident varTable,
     varTable, methTable, labelTable)
translateStmt (SDec (EArrRef t ident expr)) varTable methTable labelTable
  = (translateVariable (lookupVarTableEntry ident varTable) ++
     [Dup]                                                  ++
     exprInstrs                                             ++
     [Dup]                                                  ++
     [IStore tempLoc]                                       ++
     [Swap]                                                 ++
     [ILoad tempLoc]                                        ++
     [IALoad]                                               ++
     [IConst_m1]                                            ++
     [IAdd]                                                 ++
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
    [Getfield (thisClass++"/_scanner") "Ljava/util/Scanner;"] ++
    [Invokevirtual ("java/util/Scanner/"++scanMeth) "" t']    ++
    charHandling                                              ++
    translateVarAssign ident varTable,
    varTable, methTable, labelTable)
      where
        t'           = translateToJTypeId t
        scanMeth     = getScanMeth t
        charHandling = inputCharHandling t
translateStmt (SCall ident (APList exprs)) varTable methTable labelTable
  = ([ALoad_0]    ++
    paramsInstrs  ++
    [Invokevirtual callString paramString returnType],
    varTable, methTable, labelTable')
      where
        (Entry ident' paramString returnType)
          = lookupMethTableEntry ident methTable
        callString
          = thisClass++"/"++ident'
        (paramsInstrs, labelTable')
          = translateParams exprs varTable methTable labelTable
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

translateVarAssign :: String -> VarTable -> JProgram
translateVarAssign ident varTable
  = translateEntryAssign (lookupVarTableEntry ident varTable) varTable
translateEntryAssign :: VarTableEntry -> VarTable -> JProgram
translateEntryAssign (Global ident t) varTable
  | t == "I"                  = standardGlobal (Global ident t)
  | t == "C"                  = standardGlobal (Global ident t)
  | t == "Ljava/lang/String;" = standardGlobal (Global ident t)
  | otherwise                 = arrayAccess (Global ident t) varTable
translateEntryAssign (Local ident loc "I") varTable
  = [(IStore loc)]
translateEntryAssign (Local ident loc "Ljava/lang/String;") varTable
  = [(AStore loc)]
translateEntryAssign (Local ident loc "C") varTable
  = [(IStore loc)]
translateEntryAssign (Local ident loc arr) varTable
  = translateEntryAssignArr loc arr

standardGlobal :: VarTableEntry -> JProgram
standardGlobal (Global ident t)
  = [ALoad_0] ++
    [Swap]    ++
    [(Putfield (thisClass++"/"++ident) t)]
arrayAccess :: VarTableEntry -> VarTable -> JProgram
arrayAccess (Global ident t) varTable
  = [ALoad_0]                            ++
    [Getfield (thisClass++"/"++ident) t] ++
    [Swap]                               ++
    [IStore loc]                         ++
    [Swap]                               ++
    [ILoad loc]                          ++
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
    labelTable'')
      where
        (exInstrs, labelTable')  = translateExpr ex varTable methTable labelTable
        (unInstrs, labelTable'') = translateUnOp op labelTable'
translateExpr (EId _ ident) varTable methTable labelTable
  = (translateVariable (lookupVarTableEntry ident varTable), labelTable)
translateExpr (EString str) varTable methTable labelTable
  = ([(Ldc (ConsS str))], labelTable)
translateExpr (EInt int) varTable methTable labelTable
  = ([Ldc (ConsI int)], labelTable)
translateExpr (EChar char) varTable methTable labelTable
  = ([BIPush (ord char)], labelTable)
translateExpr (EArrRef t ident expr) varTable methTable labelTable
  = (translateVariable entry  ++
    exInstrs                  ++
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
    [Invokevirtual callString paramString returnType],
    labelTable')
      where
        (paramsInstrs, labelTable')           = translateParams exprs varTable methTable labelTable
        (Entry ident' paramString returnType) = lookupMethTableEntry ident methTable
        callString                            = thisClass++"/"++ident'

translateParams :: [Expr] -> VarTable -> MethTable -> LabelTable -> (JProgram, LabelTable)
translateParams [] varTable methTable labelTable
  = ([], labelTable)
translateParams ((EId (Ref Number) ident):rest) varTable methTable labelTable
  = ([NewAtomicReference]   ++
    [Dup]                   ++
    makeNewVarObject        ++
    [InvokeAtomicReference] ++
    params,
    labelTable')
    where
      (params, labelTable') = translateParams rest varTable methTable labelTable
      makeNewVarObject      = translateObjectWrapper (Just Number) ident varTable

translateParams (expr:rest) varTable methTable labelTable
  = (exprInstrs ++ params, labelTable'')
      where
        (params, labelTable')      = translateParams rest varTable methTable labelTable
        (exprInstrs, labelTable'') = translateExpr expr varTable methTable labelTable'

translateObjectWrapper :: (Maybe Type) -> String -> VarTable -> JProgram
translateObjectWrapper (Just Number) ident varTable
  = [New "java/lang/Integer"] ++
    [Dup]                     ++
    identValueCode            ++
    [Invokespecial "java/lang/Integer/<init>" "I" "V"]
      where
        identValueCode = translateVariable (lookupVarTableEntry ident varTable)
translateObjectWrapper (Just Letter) ident varTable
  = [New "java/lang/Integer"] ++
    [Dup]                     ++
    identValueCode            ++
    [Invokespecial "java/lang/Integer/<init>" "I" "V"]
      where
        identValueCode = translateVariable (lookupVarTableEntry ident varTable)
translateObjectWrapper (Just Sentence) ident varTable
  = translateVariable (lookupVarTableEntry ident varTable)


translateVariable :: VarTableEntry -> JProgram
translateVariable (Global ident t)
  = [ALoad_0] ++
    [Getfield (thisClass++"/"++ident) t]
translateVariable (Local ident num "I")
  = [ILoad num]
translateVariable (Local ident num "C")
  = [ILoad num]
translateVariable (Local ident num "Ljava/lang/String;")
  = [ALoad num]
-- If not a primitive type load the reference onto the stack.
translateVariable (Local ident num t)
  = [ALoad num]

translateBinOp :: String -> LabelTable -> (JProgram, LabelTable)
translateBinOp "+" labelTable  = ([IAdd], labelTable)
translateBinOp "-" labelTable  = ([ISub], labelTable)
translateBinOp "*" labelTable  = ([IMul], labelTable)
translateBinOp "/" labelTable  = ([IDiv], labelTable)
translateBinOp "%" labelTable  = ([IRem], labelTable)
translateBinOp "|" labelTable  = ([IOr], labelTable)
translateBinOp "^" labelTable  = ([IXor], labelTable)
translateBinOp "&" labelTable  = ([IAnd], labelTable)
translateBinOp "||" labelTable = ([IOr], labelTable)
translateBinOp "&&" labelTable = ([IAnd], labelTable)
translateBinOp "==" labelTable
  = ([If_icmpeq label]   ++
    booleanCode,
    labelTable')
      where
        (booleanCode, label, labelTable') = setBooleanCode labelTable
translateBinOp "!=" labelTable
  = ([If_icmpeq label] ++
    booleanCode,
    labelTable')
      where
        (booleanCode, label, labelTable') = setBooleanCode labelTable
translateBinOp ">" labelTable
  = ([If_icmpgt label] ++
    booleanCode,
    labelTable')
      where
        (booleanCode, label, labelTable') = setBooleanCode labelTable
translateBinOp "<" labelTable
  = ([If_icmplt label] ++
    booleanCode,
    labelTable')
      where
        (booleanCode, label, labelTable') = setBooleanCode labelTable
translateBinOp ">=" labelTable
  = ([If_icmpge label] ++
    booleanCode,
    labelTable')
      where
        (booleanCode, label, labelTable') = setBooleanCode labelTable
translateBinOp "<=" labelTable
  = ([If_icmple label] ++
     booleanCode,
     labelTable')
       where
         (booleanCode, label, labelTable') = setBooleanCode labelTable

setBooleanCode :: LabelTable -> (JProgram, Label, LabelTable)
setBooleanCode labelTable
  = ([Ldc (ConsI 0)] ++
    [Goto label']   ++
    [LLabel label]  ++
    [Ldc (ConsI 1)] ++
    [LLabel label'],
    label, labelTable'')
      where
        (label, labelTable')   = generateNewLabel labelTable
        (label', labelTable'') = generateNewLabel labelTable'

-- After testing, if this works, can remove the LabelTable part.
translateUnOp :: String -> LabelTable -> (JProgram, LabelTable)
translateUnOp "-" labelTable = ([INeg], labelTable)
translateUnOp "+" labelTable = ([], labelTable)
translateUnOp "~" labelTable = ([INeg], labelTable)
translateUnOp "!" labelTable = ([(Ldc (ConsI 1)), (IXor)], labelTable)

translateForParamString :: Type -> String
translateForParamString Number      = "I"
translateForParamString Letter      = "I"
translateForParamString Sentence    = "L/java/lang/String;"
translateForParamString (RefType t) = "[" ++ translateForParamString t

translateToJType :: Type -> String
translateToJType Number       = "I"
translateToJType Letter       = "C"
translateToJType Sentence     = "Ljava/lang/String;"
translateToJType Boolean      = "C"
translateToJType (RefType t)  = "[" ++ translateToJType t
translateToJType (Ref t)      = "Ljava/util/concurrrent/atomic/AtomicReference;" 
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

-- Because we can only input strings, we take the 0th char from that string.
inputCharHandling :: Type -> JProgram
inputCharHandling Letter
  = [Ldc (ConsI 0)] ++
    [Invokevirtual "java/lang/String/sharAt" "I" "C"]
inputCharHandling _
  = []
-- Internally chars are actually stored as ints, so when printing we must conver them.
printCharHandling :: Type -> JProgram
printCharHandling Letter
  = [I2c]
printCharHandling _
  = []

type VarTable = [VarTableEntry]

data VarTableEntry
  --       Ident  Type           Ident  Loc Type
  = Global String String | Local String Int String
    deriving (Show, Eq)
lookupVarTableEntry :: String -> VarTable -> VarTableEntry
-- This should never be called.
lookupVarTableEntry str []
  = (Local str 99999 "I")
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
  = translateForParamString t ++
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
showJavaProgram program
  = putStr (getJavaProgramString program)
getJavaProgramString :: JProgram -> String
getJavaProgramString []
  = ""
getJavaProgramString ((Constructor program):rest)
  = ".method public <init>()V\n"  ++
    " .limit stack 100\n"                             ++
    " .limit locals 100\n"                            ++
    "aload_0\n"                                       ++
    "dup\n"                                           ++
    "invokespecial java/lang/Object/<init>()V\n"      ++
    constructorCode                                    ++
    "invokevirtual " ++ thisClass ++ "/hatta()V\n"    ++
    "return\n"                                        ++
    ".end method\n"                                   ++
    rest'
      where
        constructorCode = getJavaProgramString program
        rest'           = getJavaProgramString rest
getJavaProgramString (instr:rest)
  = (show instr) ++ (getJavaProgramString rest)

moveFieldsToTop :: JProgram -> JProgram
moveFieldsToTop program
  = fields ++ rest
    where
      (fields, rest) = moveFieldsToTop' program
moveFieldsToTop' :: JProgram -> (JProgram, JProgram)
moveFieldsToTop' []
  = ([], [])
moveFieldsToTop' ((Field label t):rest)
  = (((Field label t):fields), rest')
    where
      (fields, rest') = moveFieldsToTop' rest
moveFieldsToTop' (instr:rest)
  = (fields, instr:rest')
    where
      (fields, rest') = moveFieldsToTop' rest

mergeConstructors :: JProgram -> JProgram
mergeConstructors program
  = [Constructor (constructors)] ++ rest
    where
      (constructors, rest) = mergeConstructors' program
mergeConstructors' :: JProgram -> (JProgram, JProgram)
mergeConstructors' []
  = ([], [])
mergeConstructors' ((Constructor program):rest)
  = ((program++cons), rest')
    where
      (cons, rest') = mergeConstructors' rest
mergeConstructors' (instr:rest)
  = (cons, instr:rest')
    where
      (cons, rest') = mergeConstructors' rest

type LabelTable = [String]
generateNewLabel :: LabelTable -> (String, LabelTable)
generateNewLabel labelTable
  = generateNewLabel' labelTable 0
generateNewLabel' :: LabelTable -> Int -> (String, LabelTable)
generateNewLabel' labelTable int
  | elem label labelTable           = generateNewLabel' labelTable (int+1)
  | otherwise                       = (label, label:labelTable)
    where
      label = "_label" ++ show int

getJunkLabels :: JProgram -> LabelTable
getJunkLabels []
  = []
getJunkLabels ((LLabel label):(Endmethod):rest)
  = (label):(getJunkLabels rest)
getJunkLabels (somethingElse:rest)
  = getJunkLabels rest

removeJunkLabels :: JProgram -> LabelTable -> JProgram
removeJunkLabels [] labelTable
  = []
removeJunkLabels ((LLabel label):rest) labelTable
  | elem label labelTable = removeJunkLabels rest labelTable
  | otherwise             = (LLabel label):(removeJunkLabels rest labelTable)
removeJunkLabels ((Goto label):rest) labelTable
  | elem label labelTable = removeJunkLabels rest labelTable
  | otherwise             = (Goto label):(removeJunkLabels rest labelTable)
removeJunkLabels (somethingElse:rest) labelTable
  = (somethingElse):(removeJunkLabels rest labelTable)

usesInput :: JProgram -> Bool
usesInput []
  = False
usesInput ((Getfield meth "Ljava/util/Scanner;"):_)
  = True
    where
      meth = thisClass++"/_scanner"
usesInput (_:rest)
  = usesInput rest

inputConstructor :: JProgram
inputConstructor
  = [Field "_scanner" "Ljava/util/Scanner;"] ++
    [Constructor
     ([ALoad_0]                                                             ++
     [New "java/util/Scanner"]                                              ++
     [Dup]                                                                  ++
     [Getstatic "java/lang/System.in" "Ljava/io/InputStream;"]              ++
     [Invokespecial "java/util/Scanner/<init>" "Ljava/io/InputStream;" "V"] ++
     [Putfield (thisClass++"/_scanner") "Ljava/util/Scanner;"])]

setupInputIfRequired :: JProgram -> JProgram
setupInputIfRequired program
  | usesInput program = inputConstructor ++ program
  | otherwise         = program
