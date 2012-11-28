module MAlice.SemanticAnalysis.ExprChecker
       ( checkExpr
       , inferType
       , TestResult(..)
       , getArrayType
       , getIdType
       , succeed
       , fail
       ) where

import MAlice.Language.AST
import MAlice.Language.SymbolTable
import MAlice.Language.Types
import MAlice.Language.Utilities
import MAlice.Parsing.ParserState
import Prelude hiding (fail)
import Control.Monad hiding (fail)

type TestResult = Either String Type

succeed :: Type -> TestResult
succeed = Right

-- |Indicates an error during type-checking of an expression
fail :: (Show a) => String -> a -> TestResult
fail s t = Left ("Expected " ++ s ++ ", got " ++ show t)

-- |Check the type for a unary numerical operation
testUNumOp :: Type -> TestResult
testUNumOp t =
  if (isNum t)
  then succeed t
  else fail "a numeric type" t

-- |Check the type for a binary numerical operation. Types need to match
-- and be numerical.
testBNumOp :: Type -> Type -> TestResult
testBNumOp t1 t2 =
  if (isNum t1 && isNum t2 && t1 == t2)
  then succeed t1
  else fail "numeric types on binary numeric operation" (t1, t2)

-- |Check the type for a unary boolean operation
testUBoolOp :: Type -> TestResult
testUBoolOp t =
  if t == Boolean
  then succeed Boolean
  else fail "type Boolean" t

-- |Check the type for a binary boolean operation
testBBoolOp :: Type -> Type -> TestResult
testBBoolOp t1 t2 =
  if (t1 == Boolean && t2 == Boolean)
  then succeed Boolean
  else fail "two boolean expressions on boolean operation" (t1, t2)

-- |Check the type for a relational operation
testRelOp :: Type -> Type -> TestResult
testRelOp t1 t2 =
  if (isOrd t1 && isOrd t2 && t1 == t2)
  then succeed Boolean
  else fail "two equal, ordered types on relational operation" (t1, t2)

-- |Check the type for an equality test
testEqOp :: Type -> Type -> TestResult
testEqOp t1 t2 =
  if (isEq t1 && isEq t2 && t1 == t2)
  then succeed Boolean
  else fail "two equal, comparable types" (t1, t2)

-- |Infers the type of an expression. This returns Maybe Type so the type
-- checker can stop checking expressions with known type errors in them.
-- Since procedures are specified as returning Nothing, expressions with
-- procedure calls automatically fail.
inferType :: Expr -> MParser (Maybe Type)
inferType ex =
  case ex of
    EPlus e1 e2  -> inferBinary testBNumOp e1 e2 ex
    EMinus e1 e2 -> inferBinary testBNumOp e1 e2 ex
    EMult e1 e2  -> inferBinary testBNumOp e1 e2 ex
    EDiv e1 e2   -> inferBinary testBNumOp e1 e2 ex
    EMod e1 e2   -> inferBinary testBNumOp e1 e2 ex
    EBAnd e1 e2  -> inferBinary testBNumOp e1 e2 ex
    EBOr e1 e2   -> inferBinary testBNumOp e1 e2 ex
    EBXor e1 e2  -> inferBinary testBNumOp e1 e2 ex
    ELOr e1 e2   -> inferBinary testBBoolOp e1 e2 ex
    ELAnd e1 e2  -> inferBinary testBBoolOp e1 e2 ex
    EGT e1 e2    -> inferBinary testRelOp e1 e2 ex
    EGTE e1 e2   -> inferBinary testRelOp e1 e2 ex
    ELTE e1 e2   -> inferBinary testRelOp e1 e2 ex
    ELT e1 e2    -> inferBinary testRelOp e1 e2 ex
    EEq e1 e2    -> inferBinary testEqOp e1 e2 ex
    ENEq e1 e2   -> inferBinary testEqOp e1 e2 ex
    ENot e1      -> inferUnary testUBoolOp e1 ex
    EInv e1      -> inferUnary testUNumOp e1 ex
    ENegate e1   -> inferUnary testUNumOp e1 ex
    EPositive e1 -> inferUnary testUNumOp e1 ex
    EId var      -> getIdType var IdVariable ex
    EString _    -> return (Just Sentence)
    EInt _       -> return (Just Number)
    EChar _      -> return (Just Letter)
    EArrRef v _  -> getArrayType v ex
    EBkt expr    -> inferType expr
    ECall f args -> getIdType f IdFunction ex

-- |Helper function used to type-check unary expressions.
-- To be used in conjunction with any of the testU##### functions.
inferUnary :: (Type -> TestResult) -> Expr -> Expr -> MParser (Maybe Type)
inferUnary test e1 src = do
  t1 <- inferType e1
  setContext . show $ src
  maybeCheck (return Nothing) t1 $ \t1' ->
    case test t1' of
      Right t  -> return (Just t)
      Left msg -> (logError . TypeError $ msg) >> return Nothing

-- |Helper function used to type-check binary expressions.
-- To be used in conjunction with any of the testB##### functions.
inferBinary :: (Type -> Type -> TestResult) ->
               Expr -> Expr -> Expr -> MParser (Maybe Type)
inferBinary test e1 e2 src = do
  t1 <- inferType e1
  t2 <- inferType e2
  setContext . show $ src
  maybeCheck2 (return Nothing) t1 t2 $ \t1' t2' ->
    case (test t1' t2') of
      Right t  -> return (Just t)
      Left msg -> (logError . TypeError $ msg) >> return Nothing

-- |Finds the type of an identifier. If it's not found or isn't of the kind
-- IdVariable, errors are logged and a Nothing is returned so type checking
-- on this expression can't continue.
getIdType :: String -> IdentifierType -> Expr -> MParser (Maybe Type)
getIdType var expected src = do
  setContext . show $ src
  v <- findGlobalIdentifier var
  case v of
    Nothing ->
      (logError . UnknownIdentifierError $ var) >> return Nothing
    Just e  ->
      if idType e == expected
      then return . returnType $ e
      else (logError . InvalidIdKindError $ var ++ " is of kind " ++
           show (idType e) ++ ", expected " ++ show expected)
           >> return Nothing

-- |Finds the item type of an array. If the identifier doesn't refer to an array
-- , an error is logged.
getArrayType :: String -> Expr -> MParser (Maybe Type)
getArrayType var src = do
  t <- getIdType var IdVariable src
  maybeCheck (return Nothing) t $ \t' ->
    case t' of
      (RefType t'') -> return (Just t'')
      _             -> (logError . TypeError $
                        "Expected reference (array) type, got " ++ show t)
                       >> return Nothing

-- |Checks if an expression has the expected type, if not an error is logged.
checkExpr :: Type -> Expr -> String -> MParser ()
checkExpr expected expr context = do
  actual <- inferType expr
  -- Set context after type checking the expression
  setContext context
  maybeCheck (return ()) actual $ \actual' ->
    if expected == actual'
    then return ()
    else logError . TypeError $
         "Expected " ++ show expected ++ ", got " ++ show actual'