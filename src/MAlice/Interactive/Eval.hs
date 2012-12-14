module MAlice.Interactive.Eval where

import Control.Monad.State
import Data.Bits
import Data.Char
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as V
import MAlice.Language.AST
import MAlice.Language.Types
import MAlice.Interactive.Types
import MAlice.SemanticAnalysis.ExprChecker
import System.IO

evalGlobals :: Program -> MEval ()
evalGlobals (Program (DeclList ds)) =
  mapM_ runDecl ds

runDecl :: Decl -> MEval ()
runDecl (VarDecl t i) =
  newVar t i
runDecl (VAssignDecl t i e) = do
  newVar t i
  assignExpr t i e
runDecl (VArrayDecl t i e) = do
  n <- evalIntExpr e
  newArray t i n
runDecl fd@(FuncDecl i _ _ _) =
  newDecl i fd
runDecl pd@(ProcDecl i _ _) =
  newDecl i pd

assignExpr :: Type -> String -> Expr -> MEval ()
assignExpr t i e =
  case t of
    Number   -> assignIntExpr i e
    Letter   -> assignChrExpr i e
    Sentence -> assignStrExpr i e
    Boolean  -> assignBolExpr i e

assignArrayExpr :: String -> Expr -> Expr -> MEval ()
assignArrayExpr i ix e = do
  ix' <- evalIntExpr ix
  (ArrVar vect) <- getVar i
  e' <- evalExpr e
  let newV = V.modify (\v -> V.write v ix' e') vect
  setVar i (ArrVar newV)

assignIntExpr :: String -> Expr -> MEval ()
assignIntExpr i e = do
  val <- evalIntExpr e
  setVar i (IntVar $ Just val)

assignChrExpr :: String -> Expr -> MEval ()
assignChrExpr i e = do
  val <- evalChrExpr e
  setVar i (ChrVar $ Just val)

assignStrExpr :: String -> Expr -> MEval ()
assignStrExpr i e = do
  val <- evalStrExpr e
  setVar i (StrVar $ Just val)

assignBolExpr :: String -> Expr -> MEval ()
assignBolExpr i e = do
  val <- evalBoolExpr e
  setVar i (BolVar $ Just val)

hOp :: [(String, a)] -> String -> a
hOp t s = fromJust (lookup s t)

intBinOps =
  [ ("+", (+))
  , ("-", (-))
  , ("*", (*))
  , ("/", div)
  , ("%", mod)
  , ("&", (.&.))
  , ("|", (.|.))
  , ("^", xor) ]

isIntBinOp = flip elem (map fst intBinOps)

intUnOps =
  [ ("+", id)
  , ("-", negate) ]

isIntUnOp = flip elem (map fst intUnOps)

intRelOps =
  [ ("<", (<))
  , (">", (>))
  , (">=", (>=))
  , ("<=", (<=)) ]

isIntRelOp = flip elem (map fst intRelOps)

eqOps :: Eq a => [(String, a -> a -> Bool)]
eqOps =
  [ ("==", (==))
  , ("!=", (/=)) ]

isEqOp :: String -> Bool
isEqOp = flip elem ["==", "!="]

boolOps =
  [ ("&&", (&&))
  , ("||", (||)) ]

isBoolOp = flip elem (map fst boolOps)

evalIntExpr :: Expr -> MEval Int
evalIntExpr (EBinOp op e1 e2) | isIntBinOp op = do
  v1 <- evalIntExpr e1
  v2 <- evalIntExpr e2
  let f = hOp intBinOps op
  return $ v1 `f` v2
evalIntExpr (EUnOp op e) | isIntUnOp op = do
  v <- evalIntExpr e
  let f = hOp intUnOps op
  return $ f v
evalIntExpr (EInt i) =
  return i
evalIntExpr (EId Number s) = do
  (IntVar (Just i)) <- getVar s
  return i
evalIntExpr (EArrRef Number i ix) = do
  ix' <- evalIntExpr ix
  (IntVar (Just i)) <- getArrElem i ix'
  return i
evalIntExpr (ECall Number f (APList ps)) = do
  eps <- mapM evalExpr ps
  (IntVar (Just i)) <- callF f eps
  return i

evalChrExpr :: Expr -> MEval Char
evalChrExpr (EBinOp op e1 e2) | isIntBinOp op = do
  v1 <- evalChrExpr e1
  v2 <- evalChrExpr e2
  let f = hOp intBinOps op
  return . chr $ (ord v1) `f` (ord v2)
evalChrExpr (EUnOp op e) | isIntUnOp op = do
  v <- evalChrExpr e
  let f = hOp intUnOps op
  return . chr $ f (ord v)
evalChrExpr (EChar i) =
  return i
evalChrExpr (EId Letter s) = do
  (ChrVar (Just i)) <- getVar s
  return i
evalChrExpr (EArrRef Letter i ix) = do
  ix' <- evalIntExpr ix
  (ChrVar (Just i)) <- getArrElem i ix'
  return i
evalChrExpr (ECall Letter f (APList ps)) = do
  eps <- mapM evalExpr ps
  (ChrVar (Just i)) <- callF f eps
  return i

evalBoolExpr :: Expr -> MEval Bool
evalBoolExpr (EBinOp op e1 e2)
  | isEqOp op = do v1 <- evalExpr e1
                   v2 <- evalExpr e2
                   let f = hOp eqOps op
                   return $ v1 `f` v2
  | isIntRelOp op = do v1 <- evalIntExpr e1
                       v2 <- evalIntExpr e2
                       let f = hOp intRelOps op
                       return $ v1 `f` v2
  | isBoolOp op = do v1 <- evalBoolExpr e1
                     v2 <- evalBoolExpr e2
                     let f = hOp boolOps op
                     return $ v1 `f` v2
evalBoolExpr (EUnOp "!" e) = do
  v <- evalBoolExpr e
  return $ not v
evalBoolExpr (EArrRef Boolean i ix) = do
  ix' <- evalIntExpr ix
  (BolVar (Just i)) <- getArrElem i ix'
  return i
evalBoolExpr (EId Boolean i) = do
  (BolVar (Just i)) <- getVar i
  return i
evalBoolExpr (ECall Boolean f (APList ps)) = do
  eps <- mapM evalExpr ps
  (BolVar (Just i)) <- callF f eps
  return i

evalStrExpr :: Expr -> MEval String
evalStrExpr (EId Sentence i) = do
  (StrVar (Just s)) <- getVar i
  return s
evalStrExpr (EArrRef Sentence i ix) = do
  ix' <- evalIntExpr ix
  (StrVar (Just i)) <- getArrElem i ix'
  return i
evalStrExpr (ECall Sentence f (APList ps)) = do
  eps <- mapM evalExpr ps
  (StrVar (Just i)) <- callF f eps
  return i
evalStrExpr (EString s) =
  return s

evalArrExpr :: Expr -> MEval (V.Vector Var)
evalArrExpr (EId (RefType _) i) = do
  (ArrVar a) <- getVar i
  return a

evalExpr :: Expr -> MEval Var
evalExpr e =
  case inferTypeP e of
    Number    -> (return . IntVar . Just) =<< evalIntExpr  e
    Letter    -> (return . ChrVar . Just) =<< evalChrExpr  e
    Sentence  -> (return . StrVar . Just) =<< evalStrExpr  e
    Boolean   -> (return . BolVar . Just) =<< evalBoolExpr e
    RefType _ -> (return . ArrVar)        =<< evalArrExpr  e

callF :: String -> [Var] -> MEval Var
callF f args = do
  (MDecl (FuncDecl _ fps _ b)) <- getVar f
  enterMethod args fps
  (Just ret) <- runBody b
  exitMethod
  return ret

runBody :: Body -> MEval (Maybe Var)
runBody (EmptyBody) = return Nothing
runBody (StmtBody cst) = runCompoundStmt cst
runBody (DeclBody (DeclList ds) cst) = mapM_ runDecl ds >> runCompoundStmt cst

runCompoundStmt :: CompoundStmt -> MEval (Maybe Var)
runCompoundStmt (CSList ss) =
  rcs ss
  where
    rcs [] = return Nothing
    rcs (s:ss) = do !sRes <- runStmt s
                    if sRes == Nothing
                      then rcs ss
                      else return sRes

runStmt :: Stmt -> MEval (Maybe Var)
runStmt (SBody b) = do
  enterBlock
  bRes <- runBody b
  exitBlock
  return bRes
runStmt (SNull) =
  return Nothing
runStmt (SAssign e1 e2) = do
  case e1 of
    (EId t i)       -> assignExpr t i e2
    (EArrRef t i e) -> assignArrayExpr i e e2
  return Nothing
runStmt (SInc e1) = do
  case e1 of
    (EId t i)       -> assignExpr t i (EBinOp "+" e1 (EInt 1))
    (EArrRef t i e) -> assignArrayExpr i e (EBinOp "+" e1 (EInt 1))
  return Nothing
runStmt (SDec e1) = do
  case e1 of
    (EId t i)       -> assignExpr t i (EBinOp "-" e1 (EInt 1))
    (EArrRef t i e) -> assignArrayExpr i e (EBinOp "-" e1 (EInt 1))
  return Nothing
runStmt (SReturn e) = do
  rVar <- evalExpr e
  return (Just rVar)
runStmt (SPrint e) = do
  val <- evalExpr e
  liftIO . putStr $ show val
  liftIO . hFlush $ stdout
  return Nothing
runStmt (SInput e) =
  case e of
    (EId t i)       -> readT t i
    (EArrRef t i e) -> readA t i e
runStmt (SCall i (APList aps)) = do
  args <- mapM evalExpr aps
  callP i args
  return Nothing
runStmt l@(SLoop cond cst) = do
  cond' <- evalBoolExpr cond
  if not cond'
    then runCompoundStmt cst >> runStmt l
    else return Nothing
runStmt (SIf ifs) =
  runIfs ifs

runIfs :: [IfClause] -> MEval (Maybe Var)
runIfs [] = return Nothing
runIfs ((If cond cst):rest) = do
  cond' <- evalBoolExpr cond
  if cond'
    then runCompoundStmt cst
    else runIfs rest
runIfs ((Else cst):rest) =
  runCompoundStmt cst

callP :: String -> [Var] -> MEval ()
callP f args = do
  (MDecl (ProcDecl _ fps b)) <- getVar f
  enterMethod args fps
  _ <- runBody b
  exitMethod

readT :: Type -> String -> MEval (Maybe Var)
readT Number var = do
  input <- liftIO getLine
  assignIntExpr var $ EInt ((read input) :: Int)
  return Nothing
readT Letter var = do
  input <- liftIO getLine
  assignChrExpr var $ EChar ((read input) :: Char)
  return Nothing
readT Sentence var = do
  input <- liftIO getLine
  assignIntExpr var $ EString input
  return Nothing
readT Boolean var = do
  input <- liftIO getLine
  let rBool "truth" = True
      rBool "lie"   = False
  assignIntExpr var $ EBool (rBool input)
  return Nothing

readA :: Type -> String -> Expr -> MEval (Maybe Var)
readA Number var ix = do
  input <- liftIO getLine
  assignArrayExpr var ix $ EInt ((read input) :: Int)
  return Nothing
readA Letter var ix = do
  input <- liftIO getLine
  assignArrayExpr var ix $ EChar ((read input) :: Char)
  return Nothing
readA Sentence var ix = do
  input <- liftIO getLine
  assignArrayExpr var ix $ EString input
  return Nothing
readA Boolean var ix = do
  input <- liftIO getLine
  let rBool "truth" = True
      rBool "lie"   = False
  assignArrayExpr var ix $ EBool (rBool input)
  return Nothing