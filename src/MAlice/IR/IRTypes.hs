module MAlice.IR.Types where

import qualified MAlice.Language.AST as AST
import MAlice.Language.Types

import Control.Monad.Identity
import Control.Monad.State

type IRCode = [Instr]

type CodeGen a = StateT CState Identity a

newtype CState = CState { lblCount :: Int }

initState = CState { lblCount = 0 }

globalPrefix = "__global_"
paramPrefix = "__param_"
labelPrefix - "__"

uniqueNumber = do
  st <- get
  let rval = lblCount st
  updateState $ \st -> st { lblCount = rval + 1 }
  return rval

data Instr =
  Alloc String |
  AllocArr String String |
  AllocParam String Int |
  Assign String String |
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
  return $ [Alloc vname] ++ code ++ [Assign vname lbl] ++ generateGlobals ds
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
-- Other cases are dealt with in generateGlobals
generateMethods (d:ds) = generateMethods ds


generateFPs :: String -> FormalParams -> CodeGen [(String, IRCode)]
generateFPs f (FPList ps) =
  mapM (generateFP f) (zip ps [0..])

generateFP :: String -> (FormalParam, Int) -> CodeGen (String, IRCode)
generateFP f ((Param t var), ix) = do
  uid <- uniqueNumber
  return $ AllocParam (paramPrefix ++ f ++ "_" ++ var ++ uid) ix