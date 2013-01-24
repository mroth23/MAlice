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

-- Converts a program in ast and a string for the name to 
-- a program made of bytecode instructons.
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

-- Translates a list of global declerations, 
-- returns a tuple that contains the instructions
-- plus new states for various tables.
translateGlobalDecls :: [Decl] -> VarTable -> MethTable -> LabelTable 
                               -> (JProgram, VarTable, MethTable, LabelTable)
translateGlobalDecls [] varTable methTable labelTable
  = ([], varTable, methTable, labelTable)
translateGlobalDecls (decl:rest) varTable methTable labelTable
  = (decl' ++ rest', varTable'', methTable'', labelTable'')
      where
        (decl', varTable', methTable', labelTable')    
	  = translateGlobalDecl decl varTable methTable labelTable
        (rest', varTable'', methTable'', labelTable'') 
	  = translateGlobalDecls rest varTable' methTable' labelTable'

-- Translates a list of local declerations, 
-- returns a tuple that contains the instructions
-- plus new states for various tables.
translateLocalDecls :: [Decl] -> VarTable -> MethTable -> LabelTable 
                              -> (JProgram, VarTable, MethTable, LabelTable)
translateLocalDecls [] varTable methTable labelTable
  = ([], varTable, methTable, labelTable)
translateLocalDecls (decl:rest) varTable methTable labelTable
  = (decl' ++ rest', varTable'', methTable'', labelTable'')
      where
        (decl', varTable', methTable', labelTable')    
	  = translateLocalDecl decl varTable methTable labelTable
        (rest', varTable'', methTable'', labelTable'') 
	  = translateLocalDecls rest varTable' methTable' labelTable'

-- Translates a list of global decleration,
-- returns a tuple that contains the instructions
-- plus new states for various tables.
translateGlobalDecl :: Decl -> VarTable -> MethTable -> LabelTable 
                            -> (JProgram, VarTable, MethTable, LabelTable)
-- Global variable decleration, unitialised.
translateGlobalDecl (VarDecl t ident) varTable methTable labelTable
  = ([Field ident t'], ((Global ident t'):varTable), methTable, labelTable)
    where
      t' = translateToJType t
-- Global variable decleration with assignment, type Sentence.
translateGlobalDecl (VAssignDecl (Sentence) ident expr) 
                                            varTable methTable labelTable
  = ([Field ident t'] ++
     [(Constructor
       ([ALoad_0]                                                         ++
       [New "java/lang/String"]                                           ++
       [Dup]                                                              ++
       exprInstrs                                                         ++
       [Invokespecial "java/lang/String/<init>" "Ljava/lang/String;" "V"] ++
       [Putfield (getClassName++"/"++ident) t'])
      )],
     (Global ident t'):varTable, methTable, labelTable')
       where
         t'                        
	   = translateToJType Sentence
         (exprInstrs, labelTable')  
	   = translateExpr expr varTable methTable labelTable
-- Global variable decleration with assignment, not a Sentence.
translateGlobalDecl (VAssignDecl t ident expr) varTable methTable labelTable
  = ([Field ident t'] ++
     [(Constructor
       ([ALoad_0]                              ++
       exprIntrs                               ++
       [Putfield (getClassName++"/"++ident) t'])
      )],
     (Global ident t'):varTable, methTable, labelTable')
       where
         t'                       
	   = translateToJType t
         (exprIntrs, labelTable') 
	   = translateExpr expr varTable methTable labelTable
-- Global array decleration.
translateGlobalDecl (VArrayDecl t ident expr) varTable methTable labelTable
  = ([Field ident t''] ++
     [(Constructor
       ([ALoad_0]                              ++
       exprInstrs                              ++
       getArrayInstruction t                   ++
       [Putfield (getClassName++"/"++ident) t''])
      )],
     (Global ident t''):varTable, methTable, labelTable')
    where
      t'                        
        = translateToAType t
      t''                       
        = "[" ++translateToJType t
      (exprInstrs, labelTable') 
        = translateExpr expr varTable methTable labelTable
-- Function decleration, has return type, contains body code.
translateGlobalDecl (FuncDecl ident (FPList formalParams) t body) 
                                              varTable methTable labelTable
  = ([Func ident param returnString numParams] ++
     bodyInstrs                                ++
     [Endmethod],
     varTable, methTable', labelTable'')
       where
         methTable'                 
	   = (Entry ident param returnString):methTable
         varTable'                  
	   = moveParamsToLocals formalParams varTable 1
         numParams                  
	   = getNumParams formalParams
         param                      
	   = makeFormalParamTypeString formalParams
         returnString               
	   = makeReturnString t
         (bodyInstrs, labelTable'') 
	   = translateBody body varTable' methTable' labelTable
-- Translate procedure, no return, contains body code.
translateGlobalDecl (ProcDecl ident (FPList formalParams) body) 
                                            varTable methTable labelTable
  = ([Func ident param "V" numParams]           ++
     bodyInstrs                                 ++
     [Return]                                   ++
     [Endmethod],
     varTable, methTable', labelTable')
       where
         methTable'                
	   = (Entry ident param "V"):methTable
         varTable'                 
	   = moveParamsToLocals formalParams varTable 1
         numParams                 
	   = getNumParams formalParams
         param                    
	   = makeFormalParamTypeString formalParams
         (bodyInstrs, labelTable') 
	   = translateBody body varTable' methTable' labelTable

-- Returns the number of parameters in a formal list.
getNumParams :: [FormalParam] -> Int
getNumParams fp = length fp

-- Sets up the current VarTable to have entries for where the 
-- parameters will be stored going into a function call.
moveParamsToLocals :: [FormalParam] -> VarTable -> Int -> VarTable
moveParamsToLocals [] varTable num
  = varTable
moveParamsToLocals (param:rest) varTable num
  = (param'++rest'++varTable)
    where
      (num', param') = moveParamToLocal param varTable num
      rest'          = moveParamsToLocals rest varTable num'

-- Actually modifies the VarTable given a formal parameter.
moveParamToLocal :: FormalParam -> VarTable -> Int -> (Int, VarTable)
moveParamToLocal (Param t ident) varTable num
  = (num+1, [Local ident num t'])
    where
      t' = translateToJType t

-- Translates a local decleration,
-- returns the instructions in a tuple
-- along with updated tables.
translateLocalDecl  :: Decl -> VarTable -> MethTable -> LabelTable 
                            -> (JProgram, VarTable, MethTable, LabelTable)
-- Variable decleration, unitialised.
translateLocalDecl (VarDecl t ident) varTable methTable labelTable
  = ([], (Local ident num t'):varTable, methTable, labelTable)
    where
      t'  = translateToJType t
      num = getNewLocalVar varTable 2
-- Sentence assignment decleration.
translateLocalDecl (VAssignDecl (Sentence) ident expr) 
                                 varTable methTable labelTable
  = ([New "java/lang/String"]                                           ++
     [Dup]                                                              ++
     exprInstrs                                                         ++
     [Invokespecial "java/lang/String/<init>" "Ljava/lang/String;" "V"] ++
     [AStore num],
     (Local ident num t'):varTable, methTable, labelTable')
       where
         t'                        
	   = translateToJType Sentence
         num                       
	   = getNewLocalVar varTable 2
         (exprInstrs, labelTable') 
	   = translateExpr expr varTable methTable labelTable
-- Local assignment decleration, not a Sentence.
translateLocalDecl (VAssignDecl t ident expr) varTable methTable labelTable
  = (exprInstrs ++
     [IStore num],
     (Local ident num t'):varTable, methTable, labelTable')
       where
         t'                        
	   = translateToJType t
         num                       
	   = getNewLocalVar varTable 2
         (exprInstrs, labelTable') 
	   = translateExpr expr varTable methTable labelTable
-- Local array decleration with given number of elements
-- in expr.
translateLocalDecl (VArrayDecl t ident expr) varTable methTable labelTable
  = (exprInstrs            ++
     getArrayInstruction t ++
     [AStore num],
     (Local ident num t'):varTable, methTable, labelTable')
    where
      t'                        
        = "[" ++ translateToJType t
      t''                       
        = translateToAType t
      num                       
        = getNewLocalVar varTable 2
      (exprInstrs, labelTable') 
        = translateExpr expr varTable methTable labelTable

-- Translates the body code of a function/procedure.
-- Because variables and methods lose scope after body code
-- is left, we only need to care about the state of the Labels
-- in the code. So we just return a tuple with the instructions
-- and the updated LabelTable.
translateBody :: Body -> VarTable -> MethTable -> LabelTable 
                      -> (JProgram, LabelTable)
-- List of declerations followed by a list of statements.
-- Translate the declerations and with the updated state,
-- proceed to translate the statements.
translateBody (DeclBody (DeclList decls) (CSList stmts)) 
                               varTable methTable labelTable
  = (decls' ++ stmts', labelTable'')
    where
      (decls', varTable', methTable', labelTable')    
        = translateLocalDecls decls varTable methTable labelTable
      (stmts', varTable'', methTable'', labelTable'') 
        = translateStmts stmts varTable' methTable' labelTable'
-- Just a list of statements. Translate the statements.
translateBody (StmtBody (CSList stmts)) varTable methTable labelTable
  = (stmts', labelTable')
     where
       (stmts', varTable', methTable', labelTable') 
         = translateStmts stmts varTable methTable labelTable
translateBody EmptyBody varTable methTable labelTable
  = ([], labelTable)

-- Translates a list of statements, translate one, then
-- translate the next with the updated states and so on.
translateStmts :: [Stmt] -> VarTable -> MethTable -> LabelTable 
                         -> (JProgram, VarTable, MethTable, LabelTable)
translateStmts [] varTable methTable labelTable
  = ([], varTable, methTable, labelTable)
translateStmts (stmt:rest) varTable methTable labelTable
  = (stmt' ++ rest', varTable'', methTable'', labelTable'')
     where
       (stmt', varTable', methTable', labelTable')    = translateStmt stmt varTable methTable labelTable
       (rest', varTable'', methTable'', labelTable'') = translateStmts rest varTable' methTable' labelTable'

-- Translate a given statment with tables, 
-- returns the instructions with the updated tables 
-- in a tuple.
translateStmt :: Stmt -> VarTable -> MethTable -> LabelTable 
                      -> (JProgram, VarTable, MethTable, LabelTable)
-- Statement is a body, so go translate the body.
translateStmt (SBody body) varTable methTable labelTable
  = (body', varTable, methTable, labelTable')
     where
       (body', labelTable') = translateBody body varTable methTable labelTable
-- Statement is null, it's empty.
translateStmt SNull varTable methTable labelTable
  = ([], varTable, methTable, labelTable)
-- Assignment to a variable, not an array.
translateStmt (SAssign (EId t id) ex) varTable methTable labelTable
  = (exprInstrs ++
     translateVarAssign id varTable exprType,
     varTable, methTable, labelTable')
       where
         (exprInstrs, labelTable') 
	   = translateExpr ex varTable methTable labelTable
	 exprType                  
	   = inferTypeP ex
-- Special assignment case for arrays, this could be handled in the 
-- case above, but this code will be more optimised.
translateStmt (SAssign (EArrRef t id ex1) ex2) varTable methTable labelTable
  = (translateVariable (lookupVarTableEntry id varTable) t  ++
     indexInstrs                                            ++
     exprInstrs                                             ++
     translateVarAssign id varTable exprType,
     varTable, methTable, labelTable'')
       where
         (indexInstrs, labelTable') 
	   = translateExpr ex1 varTable methTable labelTable
         (exprInstrs, labelTable'') 
	   = translateExpr ex2 varTable methTable labelTable'
	 exprType                   
	   = inferTypeP ex2
-- Increment a variable which is not an array.
translateStmt (SInc (EId t id)) varTable methTable labelTable
  = (translateVariable (lookupVarTableEntry id varTable) t ++
     [IConst_1]                                            ++
     [IAdd]                                                ++
     translateVarAssign id varTable Number,
     varTable, methTable, labelTable)
-- Increment an array element, this could've been handled by the case
-- above however this code is more optimised.
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
        tempLoc                   
	  = getNewLocalVar varTable 2
        (exprInstrs, labelTable') 
	  = translateExpr expr varTable methTable labelTable
-- Decrement a variable, not an array.
translateStmt (SDec (EId t ident)) varTable methTable labelTable
  = (translateVariable (lookupVarTableEntry ident varTable) t ++
     [IConst_m1]                                              ++
     [IAdd]                                                   ++
     translateVarAssign ident varTable Number,
     varTable, methTable, labelTable)
-- Decrement an array element, this could've been handled by the case
-- above however this code is more optimised.
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
        tempLoc                   
	  = getNewLocalVar varTable 2
        (exprInstrs, labelTable') 
	  = translateExpr expr varTable methTable labelTable
-- Return something defined in the expression ex.
translateStmt (SReturn ex) varTable methTable labelTable
  = (exprInstrs ++
    [returnInstr],
    varTable, methTable, labelTable')
        where
          (exprInstrs, labelTable') 
	    = translateExpr ex varTable methTable labelTable
          returnInstr               
	    = translateReturnInstr ex
-- Print a boolean, this is a special case for the Boolean test
translateStmt (SPrint ex) varTable methTable labelTable
  | inferTypeP ex == Boolean 
    = ([Getstatic "java/lang/System/out" "Ljava/io/PrintStream;"] ++
      exprInstrs                                                 ++
      [Ifeq label1]                                              ++
      [Ldc (ConsS "True")]                                       ++
      [Goto label2]                                              ++
      [LLabel label1]                                            ++
      [Ldc (ConsS "False")]                                      ++
      [LLabel label2]                                            ++
      [Invokevirtual "java/io/PrintStream/print" "Ljava/lang/String;" "V"],
      varTable, methTable, labelTable''')
        where
	  (exprInstrs, labelTable') 
	    = translateExpr ex varTable methTable labelTable
	  (label1, labelTable'')    
	    = generateNewLabel labelTable'
	  (label2, labelTable''')   
	    = generateNewLabel labelTable''
-- Print an expression that is not a boolean. We insert special
-- code for when converting characters from their internal
-- representation as ints.
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
-- Input something into a variable.
translateStmt (SInput (EId t ident)) varTable methTable labelTable
  = ([ALoad_0]                                                   ++
    [Getfield (getClassName++"/_scanner") "Ljava/util/Scanner;"] ++
    [Invokevirtual ("java/util/Scanner/"++scanMeth) "" t']       ++
    charHandling                                                 ++
    translateVarAssign ident varTable t,
    varTable, methTable, labelTable)
      where
        t'           = translateToJTypeId t
        scanMeth     = getScanMeth t
        charHandling = inputCharHandling t
-- Input something into an array, this could've been handled
-- in the above case, however this code is more optimised.
translateStmt (SInput (EArrRef t id expr)) varTable methTable labelTable
  = (translateVariable (lookupVarTableEntry id varTable) t       ++
    indexInstrs                                                  ++
    [ALoad_0]                                                    ++
    [Getfield (getClassName++"/_scanner") "Ljava/util/Scanner;"] ++
    [Invokevirtual ("java/util/Scanner/"++scanMeth) "" t']       ++
    charHandling                                                 ++
    translateVarAssign id varTable t,
    varTable, methTable, labelTable')
      where
        (indexInstrs, labelTable') 
	  = translateExpr expr varTable methTable labelTable
	t'                         
	  = translateToJTypeId t
	scanMeth                   
	  = getScanMeth t
	charHandling               
	  = inputCharHandling t
-- Call a function. Setup and push the parameters onto the stack,
-- call the function, then restore any reference values that we used.
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
-- Loops, while loop with reverse condition. 
-- Loop(thisThing) => While (thisThing==false)
translateStmt (SLoop expr (CSList stmts)) varTable methTable labelTable
  = ([LLabel label] ++
    exprInstrs      ++
    [Ifne endLabel] ++
    stmtsInstrs     ++
    [Goto label]    ++
    [LLabel endLabel],
    varTable', methTable', labelTable'''')
      where
        (label, labelTable')        
	  = generateNewLabel labelTable
        (endLabel, labelTable'')    
	  = generateNewLabel labelTable'
        (exprInstrs, labelTable''') 
	  = translateExpr expr varTable methTable labelTable''
        (stmtsInstrs, varTable', methTable', labelTable'''')
          = translateStmts stmts varTable methTable labelTable'''
-- Statement is a set of if clauses.  
translateStmt (SIf clauses) varTable methTable labelTable
  = (instrs++[LLabel endLabel], varTable, methTable, labelTable'')
      where
        (endLabel, labelTable')
          = generateNewLabel labelTable
        (instrs, varTable', methTable', labelTable'')
          = translateClauses clauses varTable methTable labelTable' endLabel

-- Returns the correct return type
-- for an expression.
translateReturnInstr :: Expr -> JInstr
translateReturnInstr ex
  = translateReturnInstr' (inferTypeP ex)
translateReturnInstr' :: Type -> JInstr
translateReturnInstr' Number
  = (IReturn)
translateReturnInstr' Letter
  = (IReturn)
translateReturnInstr' Boolean
  = (IReturn)
translateReturnInstr' Sentence
  = (AReturn)

-- Takes a set of if clauses and translates each of them, 
-- updating the current state as it goes.
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

-- Translates a single if clause, updates state.
translateClause :: IfClause -> VarTable -> MethTable -> LabelTable -> String 
                            -> (JProgram, VarTable, MethTable, LabelTable)
-- Clause is an if statement followed by a list of statements.
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
-- Clause is an else clause followed by a list of statements.
translateClause (Else (CSList stmts)) varTable methTable labelTable endLabel
  = translateStmts stmts varTable methTable labelTable

-- Given the variable name, the variable table and its true
-- type, give the correct instructions to assign current
-- value on the stack to this variable.
translateVarAssign :: String -> VarTable -> Type -> JProgram
translateVarAssign ident varTable t
  = translateEntryAssign (lookupVarTableEntry ident varTable) varTable t
translateEntryAssign :: VarTableEntry -> VarTable -> Type -> JProgram
-- Global variables require using 'putfield'.
-- Arrays need to get the field and use a seperate
-- array storage instruction. (AAStore or IAStore)
translateEntryAssign (Global ident t) varTable t'
  | t == "I"                  = standardGlobal (Global ident t)
  | t == "C"                  = standardGlobal (Global ident t)
  | t == "B"                  = standardGlobal (Global ident t)
  | t == "Ljava/lang/String;" = standardGlobal (Global ident t)
  | otherwise                 = arrayAccess (Global ident t) varTable
-- Local variables are in local scope with a variable location number.
translateEntryAssign (Local ident loc "I") varTable t'
  = [(IStore loc)]
translateEntryAssign (Local ident loc "Ljava/lang/String;") varTable t'
  = [(AStore loc)]
translateEntryAssign (Local ident loc "C") varTable t'
  = [(IStore loc)]
translateEntryAssign (Local ident loc "B") varTable t'
  = [(IStore loc)]
-- Atomic references require seperate code to input a new value in 
-- a wrapper object.
translateEntryAssign 
  (Local ident loc "Ljava/util/concurrent/atomic/AtomicReference;") varTable t'
    = translateLocalRefEntry (Local ident loc "") varTable t'
-- Local arrays need a special storage instruction.
translateEntryAssign (Local ident loc arr) varTable t'
  = getArrayInstr arr

-- Given that its an atomic reference lets put in a new object
-- containing the new value.
translateLocalRefEntry :: VarTableEntry -> VarTable -> Type -> JProgram
-- Numbers, letters and booleans are wrapped into an Integer object. 
-- Then put into the atomic reference.
translateLocalRefEntry (Local ident loc t) varTable t'
  | t' == Number || t' == Letter || t' == Boolean
    = [IStore num]                                       ++
      [ALoad loc]                                        ++
      [New "java/lang/Integer"]                          ++
      [Dup]                                              ++
      [ILoad num]                                        ++
      [Invokespecial "java/lang/Integer/<init>" "I" "V"] ++
      [Invokevirtual "java/util/concurrent/atomic/AtomicReference.set" 
                   "Ljava/lang/Object;" "V"]
        where
          num = getNewLocalVar varTable 1
-- Strings are already objects so we can just put the reference directly
-- into the atomic reference.
translateLocalRefEntry (Local ident loc t) varTable Sentence
  = [ALoad loc]  ++
    [Swap]       ++
    [Invokevirtual "java/util/concurrent/atomic/AtomicReference.set" 
                   "Ljava/lang/Object;" "V"]
      where
        num = getNewLocalVar varTable 1

-- Standard global put code.
standardGlobal :: VarTableEntry -> JProgram
standardGlobal (Global ident t)
  = [ALoad_0] ++
    [Swap]    ++
    [(Putfield (getClassName++"/"++ident) t)]
arrayAccess :: VarTableEntry -> VarTable -> JProgram
arrayAccess (Global ident t) varTable
  = arrayInstr
      where
        arrayInstr = getArrayInstr t
getArrayInstr :: String -> JProgram
getArrayInstr "[I" = [IAStore]
getArrayInstr "[C" = [IAStore]
getArrayInstr "[B" = [IAStore]
getArrayInstr  _   = [AAStore]

-- Translate a given expression.
-- Returns instructions and the updated label table
-- in a tuple.
translateExpr :: Expr -> VarTable -> MethTable -> LabelTable 
                      -> (JProgram, LabelTable)
-- Binary operator expression, translate ex1 to get ex1 value on top
-- of stack, then translate ex2 to get ex2 on top of stack. Then apply
-- op (operator) on those two values on top of stack.
translateExpr (EBinOp op ex1 ex2) varTable methTable labelTable
  = (ex1Instrs ++
    ex2Instrs ++
    binInstrs,
    labelTable''')
      where
        (ex1Instrs, labelTable')   
	  = translateExpr ex1 varTable methTable labelTable
        (ex2Instrs, labelTable'')  
	  = translateExpr ex2 varTable methTable labelTable'
        (binInstrs, labelTable''') 
	  = translateBinOp op labelTable''
-- Translate the expression and then apply the unary operator
-- to the value of that expression.
translateExpr (EUnOp op ex) varTable methTable labelTable
  = (exInstrs ++
    unInstrs,
    labelTable')
      where
        (exInstrs, labelTable')  
	  = translateExpr ex varTable methTable labelTable
        unInstrs                 
	  = translateUnOp op
-- Expression is a Number, Letter or Boolean
-- variable with an atomice reference type.
translateExpr (EId (Ref t) ident) varTable methTable labelTable
  | t == Number || t == Letter || t == Boolean
  = ((translateVariable (lookupVarTableEntry ident varTable) Number) ++
    [Invokevirtual "java/util/concurrent/atomic/AtomicReference.get" 
                   "" "Ljava/lang/Object;"]                          ++
    [Checkcast "java/lang/Integer"]                                  ++
    [Invokevirtual "java/lang/Integer.intValue" """I"], labelTable)
translateExpr (EId (Ref Sentence) ident) varTable methTable labelTable
  = ((translateVariable (lookupVarTableEntry ident varTable) Letter) ++
    [Invokevirtual "java/util/concurrent/atomic/AtomicReference.get" 
                    "" "Ljava/lang/Object;"]                         ++
    [Checkcast "java/lang/String"]
    , labelTable)
-- Translate a normal variable, not reference type.
translateExpr (EId t ident) varTable methTable labelTable
  = (translateVariable (lookupVarTableEntry ident varTable) t, labelTable)
-- String, so lets just directly load it.
translateExpr (EString str) varTable methTable labelTable
  = ([(Ldc (ConsS str))], labelTable)
-- Int, so lets just directly load it.
translateExpr (EInt int) varTable methTable labelTable
  = ([Ldc (ConsI int)], labelTable)
-- Char, lets convert to int format and load it.
translateExpr (EChar char) varTable methTable labelTable
  = ([BIPush (ord char)], labelTable)
-- Array of some sort, get element identified by expr.
translateExpr (EArrRef t ident expr) varTable methTable labelTable
  = (translateVariable entry t  ++
    exInstrs                    ++
    [IALoad],
    labelTable')
      where
        (exInstrs, labelTable') 
	  = translateExpr expr varTable methTable labelTable
        entry                   
	  = lookupVarTableEntry ident varTable
-- Boolean, lets extract the True/False part and load integer value of it.
translateExpr (EBool b) varTable methTable labelTable
  | b         = ([IConst_1], labelTable)
  | otherwise = ([IConst_0], labelTable)
-- Call a function with given parameters and then restore any references
translateExpr (ECall t ident (APList exprs)) varTable methTable labelTable
  = ([ALoad_0]                                        ++
    paramsInstrs                                      ++
    [Invokevirtual callString paramString returnType] ++
    restoreRefs,
    labelTable')
      where
        (paramsInstrs, labelTable', refTable) 
	  = translateParams exprs varTable methTable labelTable
        (Entry ident' paramString returnType) 
	  = lookupMethTableEntry ident methTable
        callString                            
	  = getClassName++"/"++ident
	restoreRefs                           
	  = translateRestoreRefs varTable refTable

-- Given the current variable table and a list of 
-- variable names and variables where the reference is
-- restore the values in references to correct location.
-- This will either be a global field (highly unlikely)
-- or a local variable.
translateRestoreRefs :: VarTable -> [(String, Int)] -> JProgram
translateRestoreRefs varTable []
  = []
translateRestoreRefs varTable (pair:pairs)
  = translateRestoreRef varTable pair ++
    translateRestoreRefs varTable pairs

-- Load the reference to the AtomicReference object.
translateRestoreRef :: VarTable -> (String, Int) -> JProgram
translateRestoreRef varTable (ident, num)
  = [ALoad num]                             ++
    [Invokevirtual "java/util/concurrent/atomic/AtomicReference.get" 
                   "" "Ljava/lang/Object;"] ++
    translateRestoreRefType (lookupVarTableEntry ident varTable)

translateRestoreRefType :: VarTableEntry -> JProgram
-- Type is either int, character or boolean, all of which
-- have their values stored in Integer object. Global.
translateRestoreRefType (Global ident t)
  | t == "I" || t == "C" || t == "B"
    = [Checkcast "java/lang/Integer"]                     ++
      [Invokevirtual "java/lang/Integer.intValue" "" "I"] ++
      [ALoad_0]                                           ++
      [Swap]                                              ++
      [Putfield field "I"]
        where
          field = getClassName++"/"++ident
-- Strings are directly stored in the AtomicReference. Global.
translateRestoreRefType (Global ident "Ljava/lang/String;")
  = [Checkcast "java/lang/String"] ++
    [ALoad_0]                      ++
    [Swap]                         ++
    [Putfield field t]
      where
        field = getClassName++"/"++ident
	t     = translateToJType Sentence
-- Local variable, type is int, character or boolean.
-- All use Integer as wrapper.
translateRestoreRefType (Local ident num t)
  | t == "I" || t == "C" || t == "B"
    = [Checkcast "java/lang/Integer"]                     ++
      [Invokevirtual "java/lang/Integer.intValue" "" "I"] ++
      [IStore num]
-- Strings are directly stored in the AtomicReference object.
translateRestoreRefType (Local ident num "Ljava/lang/String;")
  = [Checkcast "java/lang/String"] ++
    [AStore num]

-- Translates a list of parameters, as in puts the values of the
-- parameters on the stack.
translateParams :: [Expr] -> VarTable -> MethTable -> LabelTable
                          -> (JProgram, LabelTable, [(String, Int)])
translateParams [] varTable methTable labelTable
  = ([], labelTable, [])
-- If a reference wrap the variable into an object and load it
-- into the AtomicReference. Pass the AtomicReference to the function.
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
      (params, labelTable', refTable) 
        = translateParams rest ((Local "." num "."):varTable) 
	                                            methTable labelTable
      makeNewVarObject                
        = translateObjectWrapper t ident varTable
      num                             
        = getNewLocalVar varTable 1
-- If not a reference this doesnt need wrapping, just evaluating.
translateParams (expr:rest) varTable methTable labelTable
  = (exprInstrs ++ params, labelTable'', refTable)
      where
        (params, labelTable', refTable) 
	  = translateParams rest varTable methTable labelTable
        (exprInstrs, labelTable'')      =
	  translateExpr expr varTable methTable labelTable'

-- Wrap a variable into an object.
translateObjectWrapper :: Type -> String -> VarTable -> JProgram
-- Numbers, Letters and Booleans are wrapped into an Integer object.
translateObjectWrapper t ident varTable
  | t == Number || t == Letter || t == Boolean
    = [New "java/lang/Integer"] ++
      [Dup]                     ++
      identValueCode            ++
      [Invokespecial "java/lang/Integer/<init>" "I" "V"]
        where
          identValueCode 
	    = translateVariable (lookupVarTableEntry ident varTable) Number
-- Strings come pre-wrapped in thier own object.
translateObjectWrapper Sentence ident varTable
  = translateVariable (lookupVarTableEntry ident varTable) Sentence

-- Get the value from a VarTableEntry
translateVariable :: VarTableEntry -> Type -> JProgram
-- It's global, lets get it from the field.
translateVariable (Global ident t) t'
  = [ALoad_0] ++
    [Getfield (getClassName++"/"++ident) t]
-- It's local and its either a Number, Letter or Boolean.
-- Load it as an int.
translateVariable (Local ident num t') t
  | t' == "I" || t' == "C" || t' == "B"
    = [ILoad num]
-- It's a local string, load the reference.
translateVariable (Local ident num "Ljava/lang/String;") t
  = [ALoad num]
-- It's a reference, load reference then extract object then extract value.
translateVariable (Local ident num 
                         "Ljava/util/concurrent/atomic/AtomicReference;") t
  |t == Number || t == Letter || t == Boolean
     = [ALoad num]                                                       ++
       [Invokevirtual "java/util/concurrent/atomic/AtomicReference.get" 
                      "" "Ljava/lang/Object;"]                           ++
       [Checkcast "java/lang/Integer"]                                   ++
       [Invokevirtual "java/lang/Integer.intValue" """I"]
  | t == Sentence
      = [ALoad num]                                                      ++
        [Invokevirtual "java/util/concurrent/atomic/AtomicReference.get" 
	               "" "Ljava/lang/Object;"]                          ++
        [Checkcast "java/lang/String"] 
-- If not a primitive type load the reference onto the stack.
translateVariable (Local ident num t) t'
  = [ALoad num]

-- When giving things as parameters convert it to internal type.
translateForParamString :: Type -> String
translateForParamString Number      = "I"
translateForParamString Letter      = "I"
translateForParamString Boolean     = "I"
translateForParamString Sentence    = "Ljava/lang/String;"
translateForParamString (RefType t) = "[" ++ translateForParamString t
translateForParamString (Ref t)     
  = "Ljava/util/concurrent/atomic/AtomicReference;"

-- Type that the JVM requires for certain things
-- like printing and input.
translateToJType :: Type -> String
translateToJType Number       = "I"
translateToJType Letter       = "C"
translateToJType Sentence     = "Ljava/lang/String;"
translateToJType Boolean      = "B"
translateToJType (RefType t)  = "[" ++ translateToJType t
translateToJType (Ref t)      = 
  "Ljava/util/concurrent/atomic/AtomicReference;" 
translateToJTypeId :: Type -> String
translateToJTypeId Number   = "I"
translateToJTypeId Letter   = "C"
translateToJTypeId Boolean  = "I"
translateToJTypeId Sentence = "Ljava/lang/String;"

-- Type the array requires.
translateToAType :: Type -> String
translateToAType Number   = "int"
translateToAType Letter   = "int"
translateToAType Boolean  = "int"
translateToAType Sentence = "java/lang/String"

-- The method call for input  based on type.
getScanMeth :: Type -> String
getScanMeth Number   = "nextInt"
getScanMeth Letter   = "nextLine"
getScanMeth Sentence = "nextLine"

-- Makes the formal param type string for functions.
makeFormalParamTypeString :: [FormalParam] -> String
makeFormalParamTypeString []
  = ""
makeFormalParamTypeString ((Param t ident):rest)
  = translateForParamString t ++
    makeFormalParamTypeString rest

-- Makes the return string for functions.
makeReturnString :: Type -> String
makeReturnString t
  = translateToJType t

-- Gives the array instruction (making).
getArrayInstruction :: Type -> JProgram
getArrayInstruction t
  | t == Number || t == Letter || t == Boolean
    = [Newarray "int"]
getArrayInstruction _
  = [ANewarray "java/lang/String"]
