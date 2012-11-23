module MAlice.SemanticAnalysis.TypeChecker where


import MAlice.Language.AST
import MAlice.Language.SymbolTable
import MAlice.Language.Types
import MAlice.Parsing.ParserState

import Prelude hiding (fail)
import Control.Monad hiding (fail)

type CoerceResult = Either String Type

succeed :: Type -> CoerceResult
succeed = Right

fail :: (Show a) => String -> a -> CoerceResult
fail s t = Left ("Expected " ++ s ++ "\nActually got " ++ show t)

testUNumOp :: Type -> CoerceResult
testUNumOp t =
  if (isNum t)
  then succeed t
  else fail "a numeric type" t

testBNumOp :: Type -> Type -> CoerceResult
testBNumOp t1 t2 =
  if (isNum t1 && isNum t2 && t1 == t2)
  then succeed t1
  else fail "numeric types on binary numeric operation" (t1, t2)

testUBoolOp :: Type -> CoerceResult
testUBoolOp t =
  case t of
    Boolean -> succeed Boolean
    _       -> fail "type Boolean" t

testBBoolOp :: Type -> Type -> CoerceResult
testBBoolOp Boolean Boolean = succeed Boolean
testBBoolOp t1 t2 = fail "two Boolean types on boolean operation" (t1, t2)

testRelOp :: Type -> Type -> CoerceResult
testRelOp t1 t2 =
  if (isOrd t1 && isOrd t2 && t1 == t2)
  then succeed t1
  else fail "two Ord types on relational operation" (t1, t2)

testEqOp :: Type -> Type -> CoerceResult
testEqOp t1 t2 =
  if (isEq t1 && isEq t2 && t1 == t2)
     then succeed t1
          else fail "two equal, comparable types" (t1, t2)

testAssignOp :: Type -> Type -> CoerceResult
testAssignOp t1 t2 =
  if (t1 == t2)
  then succeed t1
  else fail "two equal types" (t1, t2)

inferType :: Expr -> MParser Type
inferType ex =
  case ex of
    EPlus e1 e2  -> inferBinary testBNumOp e1 e2
    EMinus e1 e2 -> inferBinary testBNumOp e1 e2
    EMult e1 e2  -> inferBinary testBNumOp e1 e2
    EDiv e1 e2   -> inferBinary testBNumOp e1 e2
    EMod e1 e2   -> inferBinary testBNumOp e1 e2
    EBAnd e1 e2  -> inferBinary testBNumOp e1 e2
    EBOr e1 e2   -> inferBinary testBNumOp e1 e2
    EBXor e1 e2  -> inferBinary testBNumOp e1 e2
    ELOr e1 e2   -> inferBinary testBBoolOp e1 e2
    ELAnd e1 e2  -> inferBinary testBBoolOp e1 e2
    EGT e1 e2    -> inferBinary testRelOp e1 e2
    EGTE e1 e2   -> inferBinary testRelOp e1 e2
    ELTE e1 e2   -> inferBinary testRelOp e1 e2
    ELT e1 e2    -> inferBinary testRelOp e1 e2
    EEq e1 e2    -> inferBinary testEqOp e1 e2
    ENEq e1 e2   -> inferBinary testEqOp e1 e2
    ENot e1      -> inferUnary testUBoolOp e1
    EInv e1      -> inferUnary testUNumOp e1
    ENegate e1   -> inferUnary testUNumOp e1
    EPositive e1 -> inferUnary testUNumOp e1
    EId var      -> getVarType var
    EString _    -> return Sentence
    EInt _       -> return Number
    EChar _      -> return Letter
    EArrRef v _  -> getArrayType v
    EBkt expr    -> inferType expr
    ECall f _    -> do
      fdecl <- getDecl f
      return $ returnType fdecl

getVarType :: String -> MParser Type
getVarType var = do
  ts <- getSymbolTables
  let v = lookupInTables var ts
  case v of
    Nothing -> logError . UnknownIdentifierError $ var >> return Number

getArrayType :: String -> MParser Type
getArrayType var = do
  t <- getVarType var
  case t of
    (RefType t') -> return t
    _            -> (logError . TypeError $
                    "expected reference (array) type, got " ++ show t)
                    >> return t

getDecl :: String -> MParser SymbolTableEntry
getDecl = undefined

inferUnary :: (Type -> CoerceResult) -> Expr -> MParser Type
inferUnary test e1 = do
  t1 <- inferType e1
  case test t1 of
    Right t  -> return t
    Left msg -> (logError . TypeError $ msg) >> return t1

inferBinary :: (Type -> Type -> CoerceResult) -> Expr -> Expr -> MParser Type
inferBinary test e1 e2 = do
  t1 <- inferType e1
  t2 <- inferType e2
  case (test t1 t2) of
    Right t  -> return t
    Left msg -> (logError . TypeError $ msg) >> return t1

typecheckExpr :: Type -> Expr -> MParser ()
typecheckExpr expected expr = do
  actual <- inferType expr
  if expected == actual
    then return ()
    else logError . TypeError $
         "expected " ++ show expected ++ ", got " ++ show actual
