module MAlice.CodeGeneration.AssemblyCodeGenerator where

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
  | otherwise           = [putStrLn (show (Push x))] ++ saveRegs' xs regsNotInUse

-- Restore the registers from the stack.
restRegs :: [Register] -> [IO ()]
restRegs regsNotInUse = reverse (restRegs' allRegs regsNotInUse)
restRegs' :: [Register] -> [Register] -> [IO ()]
restRegs' [] regsNotInUse = []
restRegs' (x:xs) regsNotInUse
  | elem x regsNotInUse = restRegs' xs regsNotInUse
  | otherwise           = [putStrLn (show (Pop x))] ++ restRegs' xs regsNotInUse

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
  = generateExpr expr rest -- Leave the result of the expression in the first free register, track location.
generateDecl (VArrayDecl t ident expr) regsNotInUse
  = undefined -- Make space on the stack and track location.
generateDecl (FuncDecl ident (FPList formalParams) t body) regsNotInUse
  = [putStrLn (show (Colon ident))] ++ (generateFormalParams formalParams regsNotInUse) ++ (generateBody body regsNotInUse) -- Write the function header, write the formalParams and then write the body.
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
  = [putStrLn (show (Pop r))]

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
generateStmt (SIf ((expr, comStmt):rest)) regs
  = undefined

generateExpr :: Expr -> [Register] -> [IO ()]
generateExpr (EPlus expr1 expr2) regs
  = undefined 
generateExpr (EMinus expr1 expr2) regs
  = undefined
generateExpr (EMult expr1 expr2) regs
  = undefined
generateExpr (EDiv expr1 expr2) regs
  = undefined
generateExpr (EMod expr1 expr2) regs
  = undefined
generateExpr (EBAnd expr1 expr2) regs
  = undefined
generateExpr (EBOr expr1 expr2) regs
  = undefined
generateExpr (EBXor expr1 expr2) regs
  = undefined
generateExpr (ELOr expr1 expr2) regs
  = undefined
generateExpr (ELAnd expr1 expr2) regs
  = undefined
generateExpr (EGT expr1 expr2) regs
  = undefined
generateExpr (EGTE expr1 expr2) regs
  = undefined
generateExpr (EEq expr1 expr2) regs
  = undefined
generateExpr (ELTE expr1 expr2) regs
  = undefined
generateExpr (ELT expr1 expr2) regs
  = undefined
generateExpr (ENEq expr1 expr2) regs
  = undefined
generateExpr (ENot expr) regs
  = undefined
generateExpr (EInv expr) regs
  = undefined
generateExpr (EId exp) regs
  = undefined
generateExpr (EString string) regs
  = undefined
generateExpr (EInt literal) regs
  = undefined
generateExpr (EChar char) regs
  = undefined
generateExpr (EArrRef ident expr) regs
  = undefined
generateExpr (EBkt expr) regs
  = undefined
generateExpr (ECall ident actualParams) regs
  = undefined
generateExpr (ENegate expr) regs
  = undefined
generateExpr (EPositive expr) regs
  = undefined


{- weight :: Expr -> Int
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
generateActualParams :: [Expr] -> [Register] -> [IO ()]
generateActualParams [] _ 
  = []
generateActualParams (x:xs) (r:rest)
  = generateActualParams xs (r:rest) ++ generateExpr x (r:rest) ++ [putStrLn (show (Push r))]

outputSequence :: [IO ()] -> IO ()
outputSequence [] = return ()
outputSequence (x:xs) = do x
			   outputSequence xs
