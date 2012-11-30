module MAlice.CodeGeneration.AssemblyCodeGenerator where

import Data.List
import MAlice.CodeGeneration.ASM
import MAlice.Language.AST
import MAlice.Language.Types

-- List of available registers for use.
allRegs :: [Register]
allRegs =  [RAX, RBX, RCX, RDX, R8, R9, R10, R11, R12, R13, R14]

-- Save free registers onto the stack.
saveRegs :: [Register] -> [IO ()]
saveRegs regsNotInUse = saveRegs' allRegs regsNotInUse
saveRegs' :: [Register] -> [Register] -> [IO ()]
saveRegs' [] regsNotInUse = []
saveRegs' (x:xs) regsNotInUse
  | elem x regsNotInUse = saveRegs' xs regsNotInUse
  | otherwise           = [putStrLn (show (Push (Reg x)))] ++ saveRegs' xs regsNotInUse

-- Restore the registers from the stack.
restRegs :: [Register] -> [IO ()]
restRegs regsNotInUse = reverse (restRegs' allRegs regsNotInUse)
restRegs' :: [Register] -> [Register] -> [IO ()]
restRegs' [] regsNotInUse = []
restRegs' (x:xs) regsNotInUse
  | elem x regsNotInUse = restRegs' xs regsNotInUse
  | otherwise           = [putStrLn (show (Pop (Reg x)))] ++ restRegs' xs regsNotInUse

-- Takes a program and gives and output.
generateCode :: Program -> IO ()
generateCode (Program (DeclList [])) 
  = return ()
generateCode (Program (DeclList (decl:decls)))
  = outputSequence ((generateDecl decl allRegs) ++ [generateCode (Program (DeclList decls))])

-- Turns a decleration and a list of available registers and gives a list of output.
generateDecl :: Decl -> [Register] -> [IO ()]
generateDecl (VarDecl _ _) _
  = [] -- We don't need to write code to declare a variable.
generateDecl (VAssignDecl _ ident expr) rest
  = undefined -- generateExpr expr rest -- Leave the result of the expression in the first free register, track location.
generateDecl (VArrayDecl t ident expr) regsNotInUse
  = undefined -- Make space on the stack and track location.
generateDecl (FuncDecl ident (FPList formalParams) t body) regsNotInUse
  = [putStrLn (show (Label ident))] ++ (generateFormalParams formalParams regsNotInUse) ++ (generateBody body regsNotInUse) -- Write the function header, write the formalParams and then write the body.
generateDecl (ProcDecl ident (FPList formalParams) body) regsNotInUse
  = undefined -- Write the procedure header, write the formalParams and then write the body.

-- Generates code for popping parameters from the stack into free registers.
generateFormalParams :: [FormalParam] -> [Register] ->  [IO ()]
generateFormalParams [] _ 
  = []
generateFormalParams (f:frest)  (r:rest)
  = generateFormalParam f (r:rest) ++ generateFormalParams frest rest

-- Takes a formalParametres and produces the popping code, track location.
generateFormalParam :: FormalParam -> [Register] -> [IO ()]
generateFormalParam formalParam (r:rest)
  = [putStrLn (show (Pop (Reg r)))]

-- Produces the instructions for the body code.
generateBody :: Body -> [Register] -> [IO ()]
generateBody (DeclBody decls comStmt) regs
  = undefined
generateBody (StmtBody comStmt) regs
  = undefined
generateBody EmptyBody regs
  = [return ()]

generateComStmt :: CompoundStmt -> [Register] -> [IO ()]
generateComStmt (CSList []) regs
  = [return ()]
generateComStmt (CSList (x:xs)) regs
  = generateStmt x regs ++ generateComStmt (CSList xs) regs

generateStmt :: Stmt -> [Register] -> [IO ()]
generateStmt (SBody body) regs
  = undefined
generateStmt (SNull) regs
  = undefined
-- expr1 must be (EId Ident)
generateStmt (SAssign expr1 expr2) regs
  = undefined
-- expr must be (EId Ident)
generateStmt (SInc expr) regs
  = undefined
-- expr must be (EId Ident)
generateStmt (SDec expr) regs
  = undefined
generateStmt (SReturn expr) regs
  = undefined
generateStmt (SPrint expr) regs
  = undefined
-- expr must be (EId Ident)
generateStmt (SInput expr) regs
  = undefined
generateStmt (SCall ident actualParams) regs
  = undefined
generateStmt (SLoop expr comStmt) regs
  = undefined
generateStmt (SIf treeChanged) regs
  = undefined

generateExpr :: Expr -> [Register] -> [Instruction]
generateExpr (EPlus expr1 expr2) (r1:r2:rest)
  =    (generateDualExpr expr1 expr2 (r1:r2:rest))         
    ++ [Add (Reg r1) (Reg r2)]
generateExpr (EMinus expr1 expr2) (r1:r2:rest)
  =    (generateDualExpr expr1 expr2 (r1:r2:rest))
    ++ [Sub (Reg r1) (Reg r2)]
generateExpr (EMult expr1 expr2) (r1:r2:rest)
  =    (generateDualExpr expr1 expr2 (r1:r2:rest))         
    ++ [Mul (Reg r1) (Reg r2)]
generateExpr (EDiv expr1 expr2) (r1:r2:rest)
  =    (generateDualExpr expr1 expr2 (r1:r2:rest))
    ++ [Div (Reg r1) (Reg r2)]
generateExpr (EMod expr1 expr2) (r1:r2:rest)
  =    (generateDualExpr expr1 expr2 (r1':r2':rest'))
    ++ (if elem RDX (r1:r2:rest)
        then    [IDiv (Reg r1') (Reg r2')]
             ++ [Mov  (Reg r1)  (Reg RDX)]
        else    [Push (Reg RDX)]
             ++ [IDiv (Reg r1') (Reg r2')]
             ++ [Mov  (Reg r1)  (Reg RDX)]
             ++ [Pop  (Reg RDX)] )
      where
        (r1':r2':rest') = (r1:r2:rest)\\[RDX]
generateExpr (EBAnd expr1 expr2) (r1:r2:rest)
  =    (generateDualExpr expr1 expr2 (r1:r2:rest)) 
    ++ [And (Reg r1) (Reg r2)]
generateExpr (EBOr expr1 expr2) (r1:r2:rest)
  =    (generateDualExpr expr1 expr2 (r1:r2:rest))   
    ++ [Or (Reg r1) (Reg r2)]
generateExpr (EBXor expr1 expr2) (r1:r2:rest)
  =    (generateDualExpr expr1 expr2 (r1:r2:rest))
    ++ [Xor (Reg r1) (Reg r2)]

{- ***** Need to do some defining for booleans on logical ops... ***** -}

generateExpr (ELOr expr1 expr2) (r1:r2:rest)
  = if weight expr1 >= weight expr2
    then    (generateExpr expr1 (r1:r2:rest))
         ++ (generateExpr expr2 (r2:rest))
         ++ [Or (Reg r1) (Reg r2)]
    else    (generateExpr expr2 (r1:r2:rest))
         ++ (generateExpr expr1 (r2:rest))
         ++ [Or (Reg r1) (Reg r2)]
generateExpr (ELAnd expr1 expr2) (r1:r2:rest)
  = if weight expr1 >= weight expr2
    then    (generateExpr expr1 (r1:r2:rest))
         ++ (generateExpr expr2 (r2:rest))
         ++ [And (Reg r1) (Reg r2)]
    else    (generateExpr expr2 (r1:r2:rest))
         ++ (generateExpr expr1 (r2:rest))
         ++ [And (Reg r1) (Reg r2)]
generateExpr (EGT expr1 expr2) (r1:r2:rest)
  =    (generateDualExpr expr1 expr2 (r1:r2:rest))
    ++ [Cmp (Reg r1) (Reg r2)]
    ++ [Jng "label1"]
    ++ (generateBoolTest r1 "label1" "label2")
generateExpr (EGTE expr1 expr2) (r1:r2:rest)
  = if weight expr1 >= weight expr2
    then    (generateExpr expr1 (r1:r2:rest))
         ++ (generateExpr expr2 (r2:rest))
         ++ [Cmp (Reg r1) (Reg r2)]
         ++ [Jnge "notEqual"]
         ++ [Mov (Reg r1) (Imm 1)]
         ++ [Jmp "endBoolean"]
         ++ [Label "notEqual"]
         ++ [Mov (Reg r1) (Imm 0)]
         ++ [Label "endBoolean"]
    else    (generateExpr expr2 (r2:r1:rest))
         ++ (generateExpr expr1 (r1:rest))
         ++ [Cmp (Reg r1) (Reg r2)]
         ++ [Jnge "notEqual"]
         ++ [Mov (Reg r1) (Imm 1)]
         ++ [Jmp "endBoolean"]
         ++ [Label "notEqual"]
         ++ [Mov (Reg r1) (Imm 0)]
         ++ [Label "endBoolean"]
generateExpr (EEq expr1 expr2) (r1:r2:rest)
  = if weight expr1 >= weight expr2
    then    (generateExpr expr1 (r1:r2:rest))
         ++ (generateExpr expr2 (r2:rest))
         ++ [Cmp (Reg r1) (Reg r2)]
         ++ [Jng "notEqual"]
         ++ [Mov (Reg r1) (Imm 1)]
         ++ [Jmp "endBoolean"]
         ++ [Label "notEqual"]
         ++ [Mov (Reg r1) (Imm 0)]
         ++ [Label "endBoolean"]
    else    (generateExpr expr2 (r2:r1:rest))
         ++ (generateExpr expr1 (r1:rest))
         ++ [Test (Reg r1) (Reg r2)]
generateExpr (ELTE expr1 expr2) (r1:r2:rest)
  = if weight expr1 >= weight expr2
    then    (generateExpr expr1 (r1:r2:rest))
         ++ (generateExpr expr2 (r2:rest))
         ++ [Test (Reg r1) (Reg r2)]
    else    (generateExpr expr2 (r2:r1:rest))
         ++ (generateExpr expr1 (r1:rest))
         ++ [Test (Reg r1) (Reg r2)]
generateExpr (ELT expr1 expr2) (r1:r2:rest)
  = if weight expr1 >= weight expr2
    then    (generateExpr expr1 (r1:r2:rest))
         ++ (generateExpr expr2 (r2:rest))
         ++ [Test (Reg r1) (Reg r2)]
    else    (generateExpr expr2 (r2:r1:rest))
         ++ (generateExpr expr1 (r1:rest))
         ++ [Test (Reg r1) (Reg r2)]
generateExpr (ENEq expr1 expr2) (r1:r2:rest)
  = if weight expr1 >= weight expr2 
    then    (generateExpr expr1 (r1:r2:rest))
         ++ (generateExpr expr2 (r2:rest))
         ++ [Test (Reg r1) (Reg r2)]
    else    (generateExpr expr2 (r2:r1:rest))
         ++ (generateExpr expr1 (r1:rest))
         ++ [Test (Reg r1) (Reg r2)]
generateExpr (ENot expr) (r1:rest)
  =    (generateExpr expr (r1:rest))
    ++ [Test (Reg r1) (Reg r1)]
generateExpr (EInv expr) (r:rest) 
  =    (generateExpr expr (r:rest))
    ++ [Not (Reg r)]
generateExpr (EId expr) (r:rest)
  = undefined -- Lookup in table.
generateExpr (EString string) regs
  = undefined -- How are we storing strings?
generateExpr (EInt literal) (r:rest)
  = [Mov (Reg r) (Imm literal)]
generateExpr (EChar char) regs
  = undefined
generateExpr (EArrRef ident expr) regs
  = undefined
generateExpr (EBkt expr) regs
  = undefined
generateExpr (ECall ident actualParams) regs
  = undefined {- Save regs, jump to method, Restore regs. -}
generateExpr (ENegate expr) (r:rest)
  =    (generateExpr expr (r:rest))
    ++ [Not (Reg r)]
generateExpr (EPositive expr) (r:rest)
  = generateExpr expr (r:rest)

generateDualExpr :: Expr -> Expr -> [Register] -> [Instruction]
generateDualExpr ex1 ex2 (r1:r2:rest)
  = if weight ex1 >= weight ex2
    then    generateExpr ex1 (r1:r2:rest)
         ++ generateExpr ex2 (r2:rest)
    else    generateExpr ex2 (r2:r1:rest)
         ++ generateExpr ex1 (r1:rest)
generateBoolTest :: Register -> String -> String -> [Instruction]
generateBoolTest r l1 l2
  =    [Mov (Reg r) (Imm 1)]
    ++ [Jmp l2]
    ++ [Label l1]
    ++ [Mov (Reg r) (Imm 0)]
    ++ [Label l2]

weight :: Expr -> Int
weight (EInt _)    
  = 1
weight (EString _) 
  = 1
weight (EId _)     
  = 1
weight (EChar _)   
  = 1
weight (EArrRef _ expr)
  = 1
{-
weight (ECall ident (APList exprs))
  = min (map (weight) (exprs))
weight (  expr)    
  = weight expr
weight (binary expr1 expr2)
  = min [cost1, cost2]
    where
      cost1 = max [weight expr1, (weight expr2) + 1]
      cost2 = max [(weight expr1) + 1, weight expr2] -}

-- Generate code for producing parameters and push them right to left onto stack.
generateActualParams :: [Expr] -> [Operand] -> [IO ()]
generateActualParams [] _ 
  = []
generateActualParams (x:xs) (r:rest)
  = undefined -- generateActualParams xs (r:rest) ++ generateExpr x (r:rest) ++ [putStrLn (show (Push r))]

outputSequence :: [IO ()] -> IO ()
outputSequence [] = return ()
outputSequence (x:xs) = do x
			   outputSequence xs

