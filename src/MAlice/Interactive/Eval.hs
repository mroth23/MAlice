module MAlice.Interactive.Eval where

import Control.Monad.State
import Control.Monad.Error
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

evalGlobals :: Program -> MExec ()
evalGlobals (Program (DeclList ds)) =
  mapM_ runDecl ds

runDecl :: Decl -> MExec ()
runDecl (VarDecl t i) =
  lift $ newVar t i
runDecl (VAssignDecl t i e) = do
  lift $ newVar t i
  assignExpr t i e
runDecl (VArrayDecl t i e) = do
  n <- evalIntExpr e
  lift $ newArray t i n
runDecl fd@(FuncDecl i _ _ _) =
  lift $ newDecl i fd
runDecl pd@(ProcDecl i _ _) =
  lift $ newDecl i pd

assignExpr :: Type -> String -> Expr -> MExec ()
assignExpr t i e =
  case t of
    Number   -> assignIntExpr i e
    Letter   -> assignChrExpr i e
    Sentence -> assignStrExpr i e
    Boolean  -> assignBolExpr i e
    t -> throwError $ "Eval.assignExpr: assignment to invalid type " ++ show t

assignArrayExpr :: String -> Expr -> Expr -> MExec ()
assignArrayExpr i ix e = do
  ix' <- evalIntExpr ix
  v <- lift $ getVar i
  case v of
    (ArrVar vect) -> do
      e' <- evalExpr e
      let newV = V.modify (\ve -> V.write ve ix' e') vect
      lift $ setVar i (ArrVar newV)
    _ -> throwError "Eval.assignArrayExpr: not an array variable"

assignIntExpr :: String -> Expr -> MExec ()
assignIntExpr i e = do
  val <- evalIntExpr e
  lift $ setVar i (IntVar $ Just val)

assignChrExpr :: String -> Expr -> MExec ()
assignChrExpr i e = do
  val <- evalChrExpr e
  lift $ setVar i (ChrVar $ Just val)

assignStrExpr :: String -> Expr -> MExec ()
assignStrExpr i e = do
  val <- evalStrExpr e
  lift $ setVar i (StrVar $ Just val)

assignBolExpr :: String -> Expr -> MExec ()
assignBolExpr i e = do
  val <- evalBoolExpr e
  lift $ setVar i (BolVar $ Just val)

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
  , ("-", negate)
  , ("~", complement)]

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

evalIntExpr :: Expr -> MExec Int
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
  ivar <- lift $ getVar s
  case ivar of
    (IntVar (Just i)) ->
      return i
    invalidE ->
      throwError $ "Eval.evalIntExpr: invalid expression, " ++ show invalidE
evalIntExpr (EArrRef Number i ix) = do
  ix' <- evalIntExpr ix
  ivar <- lift $ getArrElem i ix'
  case ivar of
    (IntVar (Just i)) ->
      return i
    invalidE ->
      throwError $ "Eval.evalIntExpr: invalid expression, " ++ show invalidE
evalIntExpr (ECall Number f (APList ps)) = do
  eps <- mapM evalExpr ps
  ivar <- callF f eps
  case ivar of
    (IntVar (Just i)) ->
      return i
    invalidE ->
      throwError $ "Eval.evalIntExpr: invalid expression, " ++ show invalidE
evalIntExpr e = throwError $ "Eval.evalIntExpr: invalid expression, " ++ show e

evalChrExpr :: Expr -> MExec Char
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
  ivar <- lift $ getVar s
  case ivar of
    (ChrVar (Just i)) ->
      return i
    invalidE ->
      throwError $ "Eval.evalChrExpr: invalid expression, " ++ show invalidE
evalChrExpr (EArrRef Letter i ix) = do
  ix' <- evalIntExpr ix
  ivar <- lift $ getArrElem i ix'
  case ivar of
    (ChrVar (Just i)) ->
      return i
    invalidE ->
      throwError $ "Eval.evalChrExpr: invalid expression, " ++ show invalidE
evalChrExpr (ECall Letter f (APList ps)) = do
  eps <- mapM evalExpr ps
  ivar <- callF f eps
  case ivar of
    (ChrVar (Just i)) ->
      return i
    invalidE ->
      throwError $ "Eval.evalChrExpr: invalid expression, " ++ show invalidE
evalChrExpr e = throwError $ "Eval.evalChrExpr: invalid expression, " ++ show e

evalBoolExpr :: Expr -> MExec Bool
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
  bvar <- lift $ getArrElem i ix'
  case bvar of
    (BolVar (Just i)) ->
      return i
    invalidE ->
      throwError $ "Eval.evalBoolExpr: invalid expression, " ++ show invalidE
evalBoolExpr (EId Boolean i) = do
  bvar <- lift $ getVar i
  case bvar of
    (BolVar (Just i)) ->
      return i
    invalidE ->
      throwError $ "Eval.evalBoolExpr: invalid expression, " ++ show invalidE
evalBoolExpr (ECall Boolean f (APList ps)) = do
  eps <- mapM evalExpr ps
  bvar <- callF f eps
  case bvar of
    (BolVar (Just i)) ->
      return i
    invalidE ->
      throwError $ "Eval.evalBoolExpr: invalid expression, " ++ show invalidE
evalBoolExpr e = throwError $ "Eval.evalBoolExpr: invalid expression, "++ show e

evalStrExpr :: Expr -> MExec String
evalStrExpr (EId Sentence i) = do
  svar <- lift $ getVar i
  case svar of
    (StrVar (Just s)) ->
      return s
    invalidE ->
      throwError $ "Eval.evalStrExpr: invalid expression, " ++ show invalidE
evalStrExpr (EArrRef Sentence i ix) = do
  ix' <- evalIntExpr ix
  svar <- lift $ getArrElem i ix'
  case svar of
    (StrVar (Just s)) ->
      return s
    invalidE ->
      throwError $ "Eval.evalStrExpr: invalid expression, " ++ show invalidE
evalStrExpr (ECall Sentence f (APList ps)) = do
  eps <- mapM evalExpr ps
  svar <- callF f eps
  case svar of
    (StrVar (Just s)) ->
      return s
    invalidE ->
      throwError $ "Eval.evalStrExpr: invalid expression, " ++ show invalidE
evalStrExpr (EString s) =
  return s
evalStrExpr e = throwError $ "Eval.evalStrExpr: invalid expression, " ++ show e

evalArrExpr :: Expr -> MExec (V.Vector Var)
evalArrExpr (EId (RefType _) i) = do
  avar <- lift $ getVar i
  case avar of
    (ArrVar v) ->
      return v
    invalidE ->
      throwError $ "Eval.evalArrExpr: invalid expression, " ++ show invalidE

evalExpr :: Expr -> MExec Var
evalExpr e =
  case inferTypeP e of
    Number    -> (return . IntVar . Just) =<< evalIntExpr  e
    Letter    -> (return . ChrVar . Just) =<< evalChrExpr  e
    Sentence  -> (return . StrVar . Just) =<< evalStrExpr  e
    Boolean   -> (return . BolVar . Just) =<< evalBoolExpr e
    RefType _ -> (return . ArrVar)        =<< evalArrExpr  e

callF :: String -> [Var] -> MExec Var
callF f args = do
  fId <- lift $ getVar f
  case fId of
    (MDecl (FuncDecl _ fps _ b)) -> do
      lift $ enterMethod args fps
      res <- runBody b
      lift $ exitMethod
      case res of
        (Just ret) -> return ret
        _ -> throwError "Eval.callF: reached end of function body"
    d -> throwError $ "Eval.callF: invalid identifier " ++ f

runBody :: Body -> MExec (Maybe Var)
runBody (EmptyBody) = return Nothing
runBody (StmtBody cst) = runCompoundStmt cst
runBody (DeclBody (DeclList ds) cst) = mapM_ runDecl ds >> runCompoundStmt cst

runCompoundStmt :: CompoundStmt -> MExec (Maybe Var)
runCompoundStmt (CSList ss) =
  rcs ss
  where
    rcs [] = return Nothing
    rcs (s:ss) = do sRes <- runStmt s
                    if sRes == Nothing
                      then rcs ss
                      else return sRes

runStmt :: Stmt -> MExec (Maybe Var)
runStmt (SBody b) = do
  lift enterBlock
  bRes <- runBody b
  lift exitBlock
  return bRes
runStmt (SNull) =
  return Nothing
runStmt (SAssign e1 e2) = do
  case e1 of
    (EId t i)       -> assignExpr t i e2
    (EArrRef t i e) -> assignArrayExpr i e e2
    _ -> throwError "Eval.runStmt: assignment to invalid expression"
  return Nothing
runStmt (SInc e1) = do
  case e1 of
    (EId t i)       -> assignExpr t i (EBinOp "+" e1 (EInt 1))
    (EArrRef t i e) -> assignArrayExpr i e (EBinOp "+" e1 (EInt 1))
    _ -> throwError "Eval.runStmt: increment of invalid expression"
  return Nothing
runStmt (SDec e1) = do
  case e1 of
    (EId t i)       -> assignExpr t i (EBinOp "-" e1 (EInt 1))
    (EArrRef t i e) -> assignArrayExpr i e (EBinOp "-" e1 (EInt 1))
    _ -> throwError "Eval.runStmt: decrement of invalid expression"
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
    _ -> throwError "Eval.runStmt: read in to invalid expression"
runStmt (SCall i (APList aps)) = do
  args <- mapM evalExpr aps
  callP i args
  return Nothing
runStmt l@(SLoop cond cst) = do
  cond' <- evalBoolExpr cond
  if not cond'
    then do r <- runCompoundStmt cst
            maybe (runStmt l) (return . Just) r
    else return Nothing
runStmt (SIf ifs) =
  runIfs ifs

runIfs :: [IfClause] -> MExec (Maybe Var)
runIfs [] = return Nothing
runIfs ((If cond cst):rest) = do
  cond' <- evalBoolExpr cond
  if cond'
    then runCompoundStmt cst
    else runIfs rest
runIfs ((Else cst):rest) =
  runCompoundStmt cst

callP :: String -> [Var] -> MExec ()
callP f args = do
  fd <- lift $ getVar f
  case fd of
    (MDecl (ProcDecl _ fps b)) -> do
      lift $ enterMethod args fps
      _ <- runBody b
      lift $ exitMethod
    idecl -> throwError $ "Eval.callP: invalid identifier " ++ show idecl

readT :: Type -> String -> MExec (Maybe Var)
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

readA :: Type -> String -> Expr -> MExec (Maybe Var)
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