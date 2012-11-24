module MAlice.CodeGeneration.AssemblyCodeGenerator where

import MAlice.CodeGeneration.ASM
import MAlice.Language.AST
import MAlice.Language.Types

generateCode :: Program -> IO ()
generateCode (Program (DeclList decls))
  = outputSequence (concatMap (generateDecl) (decls))

generateDecl :: Decl -> [IO ()]
generateDecl (VarDecl t ident)
  = undefined
generateDecl (VAssignDecl t ident expr)
  = undefined
generateDecl (VArrayDecl t ident expr)
  = undefined
generateDecl (FuncDecl ident (FPList formalParams) t body)
  = undefined
generateDecl (ProcDecl ident (FPList formalParams) body)
  = undefined

generateFormalParams :: [FormalParam] -> [IO ()]
generateFormalParams formalParams
  = concatMap (generateFormalParam) (formalParams)

generateFormalParam :: FormalParam -> [IO ()]
generateFormalParam formalParam
  = undefined

generateBody :: Body -> [IO ()]
generateBody (DeclBody decls comStmt)
  = undefined
generateBody (StmtBody comStmt)
  = undefined
generateBody EmptyBody
  = [return ()]

generateComStmt :: CompoundStmt -> [IO ()]
generateComStmt (CSList stmts)
  = concatMap (generateStmt) (stmts)

generateStmt :: Stmt -> [IO ()]
generateStmt (SBody body)
  = undefined
generateStmt (SNull)
  = undefined
generateStmt (SAssign expr1 expr2)
  = undefined
generateStmt (SInc expr)
  = undefined
generateStmt (SDec expr)
  = undefined
generateStmt (SReturn expr)
  = undefined
generateStmt (SPrint expr)
  = undefined
generateStmt (SInput expr)
  = undefined
generateStmt (SCall ident actualParams)
  = undefined
generateStmt (SLoop expr comStmt)
  = undefined
generateStmt (SIf ((expr, comStmt):rest))
  = undefined

generateExpr :: Expr -> [IO ()]
generateExpr (EPlus expr1 expr2)
  = undefined
generateExpr (EMinus expr1 expr2)
  = undefined
generateExpr (EMult expr1 expr2)
  = undefined
generateExpr (EDiv expr1 expr2)
  = undefined
generateExpr (EMod expr1 expr2)
  = undefined
generateExpr (EBAnd expr1 expr2)
  = undefined
generateExpr (EBOr expr1 expr2)
  = undefined
generateExpr (EBXor expr1 expr2)
  = undefined
generateExpr (ELOr expr1 expr2)
  = undefined
generateExpr (ELAnd expr1 expr2)
  = undefined
generateExpr (EGT expr1 expr2)
  = undefined
generateExpr (EGTE expr1 expr2)
  = undefined
generateExpr (EEq expr1 expr2)
  = undefined
generateExpr (ELTE expr1 expr2)
  = undefined
generateExpr (ELT expr1 expr2)
  = undefined
generateExpr (ENEq expr1 expr2)
  = undefined
generateExpr (ENot expr)
  = undefined
generateExpr (EInv expr)
  = undefined
generateExpr (EId exp)
  = undefined
generateExpr (EString string)
  = undefined
generateExpr (EInt literal)
  = undefined
generateExpr (EChar char)
  = undefined
generateExpr (EArrRef ident expr)
  = undefined
generateExpr (EBkt expr)
  = undefined
generateExpr (ECall ident actualParams)
  = undefined
generateExpr (ENegate expr)
  = undefined
generateExpr (EPositive expr)
  = undefined


generateActualParams :: [Expr] -> [IO ()]
generateActualParams exprs
  = concatMap (generateExpr) exprs

outputSequence :: [IO ()] -> IO ()
outputSequence [] = return ()
outputSequence (x:xs) = do x
			   outputSequence xs
