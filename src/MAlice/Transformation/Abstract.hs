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

-- putFunction = flip flip put . ((>>=) .) . flip flip get . M.insert
putFunction :: String -> FreeVars -> Abs ()
putFunction k e =
  M.insert k e `liftM` get >>= put

getFunction :: String -> Abs FreeVars
getFunction k =
  (fromJust . M.lookup k) `liftM` get

makeAPList :: FreeVars -> [Expr]
makeAPList fs =
  map (\(i, t) -> EId (Just . makeRefType $ t) i) fs

makeFPList :: FreeVars -> [FormalParam]
makeFPList fs =
  map (\(i, t) -> Param (makeRefType t) i) fs

makeRefType :: Type -> Type
makeRefType t@(Ref _) = t
makeRefType t@(RefType _) = t
makeRefType t = Ref t

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
  b <- abstractBody ab
  let fps = fs ++ makeFPList fv
  putFunction f fv
  return $ FuncDecl f (FPList fps) t b
abstractDecl (AProcDecl f (FPList fs) ab fv) = do
  b <- abstractBody ab
  let fps = fs ++ makeFPList fv
  putFunction f fv
  return $ ProcDecl f (FPList fps) b

abstractBody :: ABody -> Abs Body
abstractBody (AEmptyBody) =
  return EmptyBody
abstractBody (AStmtBody acst _) = do
  cst <- abstractCompoundStmt acst
  return $ StmtBody cst
abstractBody (ADeclBody ads acst _) = do
  cst <- abstractCompoundStmt acst
  ds <- abstractDecls ads
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
abstractExpr (AECall t f (AAPList aaps) fv) = do
  eargs <- getFunction f
  aps <- mapM abstractExpr aaps
  return $ ECall t f (APList $ aps ++ makeAPList eargs)