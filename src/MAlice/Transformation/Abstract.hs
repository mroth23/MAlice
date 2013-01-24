module MAlice.Transformation.Abstract
       (abstract)
       where

import Control.Monad.State
import Data.Maybe
import qualified Data.Map as M
import MAlice.Language.AST
import MAlice.Language.Types(Type(..))
import MAlice.Transformation.Types

type ArgTable = M.Map String FreeVars

type Abs a = State ArgTable a

-- Most of the functions in this module are just simple recursive bits that
-- walk the AST and add free variables as arguments to any function calls as
-- it finds them. Nothing really to explain here. Step 2/3 of the lambda lifter.

-- putFunction = flip flip put . ((>>=) .) . flip flip get . M.insert
-- puts a function definition into the state so it can be accessed whenever
-- a call to that function needs to be modified
putFunction :: String -> FreeVars -> Abs ()
putFunction k e =
  M.insert k e `liftM` get >>= put

-- get free vars from a function definition put by putFunction
getFunction :: String -> Abs FreeVars
getFunction k =
  (fromJust . M.lookup k) `liftM` get

-- some helper functions to modify function parameters
makeAPList :: FreeVars -> [Expr]
makeAPList fs =
  map (\(i, t) -> EId (makeRefType t) i) fs

makeFPList :: FreeVars -> [FormalParam]
makeFPList fs =
  map (\(i, t) -> Param (makeRefType t) i) fs

-- We don't want to ref up types that are already passed by reference
makeRefType :: Type -> Type
makeRefType t@(Ref _) = t
makeRefType t@(RefType _) = t
makeRefType t = Ref t

--Boring structural recursion
abstract :: ADecls -> Program
abstract ad = Program $ evalState (abstractDecls ad) M.empty

abstractDecls :: ADecls -> Abs Decls
abstractDecls ads = do
  ds <- mapM abstractDecl ads
  return $ DeclList ds

abstractDecl :: ADecl -> Abs Decl
abstractDecl (AVarDecl t i) =
  return $ VarDecl t i
abstractDecl (AVAssignDecl t i ae _) = do
  e <- abstractExpr ae
  return $ VAssignDecl t i e
abstractDecl (AVArrayDecl t i ae _) = do
  e <- abstractExpr ae
  return $ VArrayDecl t i e
abstractDecl (AFuncDecl f (FPList fs) t ab fv) = do
  putFunction f fv
  b <- abstractBody ab
  let fps = fs ++ makeFPList fv
  return $ FuncDecl f (FPList fps) t b
abstractDecl (AProcDecl f (FPList fs) ab fv) = do
  putFunction f fv
  b <- abstractBody ab
  let fps = fs ++ makeFPList fv
  return $ ProcDecl f (FPList fps) b

abstractBody :: ABody -> Abs Body
abstractBody (AEmptyBody) =
  return EmptyBody
abstractBody (AStmtBody acst _) = do
  cst <- abstractCompoundStmt acst
  return $ StmtBody cst
abstractBody (ADeclBody ads acst _) = do
  ds <- abstractDecls ads
  cst <- abstractCompoundStmt acst
  return $ DeclBody ds cst

abstractCompoundStmt :: ACompoundStmt -> Abs CompoundStmt
abstractCompoundStmt (ACSList as) =
  CSList `liftM` (mapM abstractStmt as)

abstractStmt :: AStmt -> Abs Stmt
abstractStmt (ASBody ab _) = do
  b <- abstractBody ab
  return $ SBody b
abstractStmt (ASNull) =
  return SNull
abstractStmt (ASAssign ae1 ae2 _) = do
  e1 <- abstractExpr ae1
  e2 <- abstractExpr ae2
  return $ SAssign e1 e2
abstractStmt (ASInc ae _) = do
  e <- abstractExpr ae
  return $ SInc e
abstractStmt (ASDec ae _) = do
  e <- abstractExpr ae
  return $ SDec e
abstractStmt (ASReturn ae _) = do
  e <- abstractExpr ae
  return $ SReturn e
abstractStmt (ASPrint ae _) = do
  e <- abstractExpr ae
  return $ SPrint e
abstractStmt (ASInput ae _) = do
  e <- abstractExpr ae
  return $ SInput e
abstractStmt (ASCall f (AAPList aaps) _) = do
  eargs <- getFunction f
  aps <- mapM abstractExpr aaps
  return $ SCall f (APList $ aps ++ makeAPList eargs)
abstractStmt (ASLoop ae acst _) = do
  e <- abstractExpr ae
  cst <- abstractCompoundStmt acst
  return $ SLoop e cst
abstractStmt (ASIf aifs _) = do
  ifs <- mapM abstractIfClause aifs
  return $ SIf ifs

abstractIfClause :: AIfClause -> Abs IfClause
abstractIfClause (AIf ae acst _) = do
  e <- abstractExpr ae
  cst <- abstractCompoundStmt acst
  return $ If e cst
abstractIfClause (AElse acst _) = do
  cst <- abstractCompoundStmt acst
  return $ Else cst

abstractExpr :: AExpr -> Abs Expr
abstractExpr (AEBinOp op ae1 ae2 _) = do
  e1 <- abstractExpr ae1
  e2 <- abstractExpr ae2
  return $ EBinOp op e1 e2
abstractExpr (AEUnOp op ae _) = do
  e <- abstractExpr ae
  return $ EUnOp op e
abstractExpr (AEId t i _) =
  return $ EId t i
abstractExpr (AEString s) =
  return $ EString s
abstractExpr (AEInt s) =
  return $ EInt s
abstractExpr (AEChar s) =
  return $ EChar s
abstractExpr (AEBool s) =
  return $ EBool s
abstractExpr (AEArrRef t i ae _) = do
  e <- abstractExpr ae
  return $ EArrRef t i e
abstractExpr (AECall t f (AAPList aaps) _) = do
  eargs <- getFunction f
  aps <- mapM abstractExpr aaps
  return $ ECall t f (APList $ aps ++ makeAPList eargs)