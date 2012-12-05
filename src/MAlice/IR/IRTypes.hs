module MAlice.IR.Types where

import qualified MAlice.Language.AST as AST
import MAlice.Language.Types

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State

type IRCode = [Instr]

type CodeGen a = StateT CState Identity a

newtype CState = CState { lblCount :: Int }

initState = CState { lblCount = 0 }

globalPrefix = "__global_"
paramPrefix = "__param_"
localPrefix = "__var_"
methodPrefix = "__m_"
labelPrefix = "__"

uniqueNumber = do
  rval <- lblCount `liftM` get
  updateState $ \st -> st { lblCount = rval + 1 }
  return rval

data Instr =
  Alloc String                        | --Allocate a new variable
  AllocArr String String              | --Allocate an array
  AllocParam String Int               | --Allocate a function param
  AssignB String String String String | --x := y `op` z
  AssignU String String String        | --x := op y
  AssignIx String String String       | --a[b] := c
  AssignAd String String String       | --a := b[c]
  AString String                      | --x := String literal
  AInt Int                            | --x := Int literal
  AChar Char                          | --x := Char literal
  Copy String String                  | --x := y
  Label String

generateIRCode :: Program -> Either String IRCode
generateIRCode (DeclList ps) =
  Right . runIdentity $ evalStateT (genDecls ps) initState

genDecls :: [Decl] -> CodeGen IRCode
genDecls ds = do
  globals <- generateGlobals ds
  methods <- generateMethods ds
  return $ globals ++ methods

generateGlobals :: [Decl] -> CodeGen IRCode
-- Base case
generateGlobals [] = return []
-- A declaration without assignment
generateGlobals ((VarDecl typ var)       : ds) =
  return $ [Alloc (globalPrefix ++ var)] ++ generateGlobals ds
-- A declaration with assignment
generateGlobals ((VAssignDecl typ var e) : ds) = do
  (lbl, code) <- generateExpr e
  let vname = globalPrefix ++ var
  return $ [Alloc vname] ++ code ++ [Copy vname lbl] ++ generateGlobals ds
-- An array declaration
generateGlobals ((VArrayDecl typ var e)  : ds) = do
  (lbl, code) <- generateExpr e
  let vname = globalPrefix ++ var
  return $ code ++ [AllocArr vname lbl] ++ generateGlobals ds
-- Other cases are dealt with in generateMethods
generateGlobals (d:ds) =
  generateGlobals ds


generateMethods :: [Decl] -> CodeGen IRCode
-- Base case
generateMethods [] = return []
-- Function declaration
generateMethods ((FuncDecl f ps _ body) : ds) = do
  paramDecls <- generateFPs f ps
  bodyCode <- generateBody body
  return $ [Label f] ++ paramDecls ++ bodyCode ++ generateMethods ds
-- Procedure declaration
generateMethods ((ProcDecl f ps body) : ds) = do
  paramDecls <- generateFPs f ps
  bodyCode <- generateBody body
  return $ [Label f] ++ paramDecls ++ bodyCode ++ generateMethods ds
-- Other cases are dealt with in generateGlobals
generateMethods (d:ds) = generateMethods ds

generateFPs :: String -> FormalParams -> CodeGen IRCode
generateFPs f (FPList ps) =
  mapM (generateFP f) (zip ps [0..])

generateFP :: String -> (FormalParam, Int) -> CodeGen Instr
generateFP f ((Param t var), ix) = do
  uid <- uniqueNumber
  return $ AllocParam (paramPrefix ++ f ++ "_" ++ var ++ uid) ix

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
generateDecl (VarDecl typ var) =
  return $ [Alloc (globalPrefix ++ var)]
-- A declaration with assignment
generateDecl (VAssignDecl typ var e) = do
  (lbl, code) <- generateExpr e
  let vname = localPrefix ++ var
  return $ [Alloc vname] ++ code ++ [Copy vname lbl]
-- An array declaration
generateDecl (VArrayDecl typ var e) = do
  (lbl, code) <- generateExpr e
  let vname = localPrefix ++ var
  return $ code ++ [AllocArr vname lbl]
-- A function declaration
generateDecl (FuncDecl f ps _ body) = do
  paramDecls <- generateFPs f ps
  bodyCode <- generateBody body
  uid <- uniqueNumber
  let fname = methodPrefix ++ f ++ "_" ++ uid
  return $ [Label fname] ++ paramDecls ++ bodyCode
-- A procedure declaration
generateDecl (ProcDecl f ps body) = do
  paramDecls <- generateFPs f ps
  bodyCode <- generateBody body
  uid <- uniqueNumber
  let fname = methodPrefix ++ f ++ "_" ++ uid
  return $ [Label fname] ++ paramDecls ++ bodyCode


generateExpr :: Expr -> CodeGen (String, IRCode)
generateExpr (EBinOp op e1 e2) = do
  (l1, e1c) <- generateExpr e1
  (l2, e2c) <- generateExpr e2
  rLbl <- uniqueLabel
  return $ e1c ++ e2c ++ [AssignB rLbl l1 op l2]
generateExpr (EUnOp op e) = do
  (l, ec) <- generateExpr e
  rLbl <- uniqueLabel
  return $ ec ++ [AssignU rLbl op l]
generateExpr (EId var) = undefined --Look up in symbol table
generateExpr (EString str) =
  return