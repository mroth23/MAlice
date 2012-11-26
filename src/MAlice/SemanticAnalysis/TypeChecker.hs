module MAlice.SemanticAnalysis.TypeChecker where

import Data.Maybe
import MAlice.Language.AST
import MAlice.Language.SymbolTable
import MAlice.Language.Types
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
  case t of
    Boolean -> succeed Boolean
    _       -> fail "type Boolean" t

-- |Check the type for a binary boolean operation
testBBoolOp :: Type -> Type -> TestResult
testBBoolOp Boolean Boolean = succeed Boolean
testBBoolOp t1 t2 = fail "two boolean expressions on boolean operation" (t1, t2)

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

-- |Check the type for an assignment operation. Reference types can't be
-- assigned to, all other types accept assignments from equal types.
testAssignOp :: Type -> Type -> TestResult
testAssignOp (RefType _) _ =
  Left "can't assign a to a reference type (const)"
testAssignOp t1 t2 =
  if (t1 == t2)
  then succeed t1
  else fail "two compatible types on assignment operation" (t1, t2)

-- |Infers the type of an expression.
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
    EId var      -> getIdType var IdVariable
    EString _    -> return Sentence
    EInt _       -> return Number
    EChar _      -> return Letter
    EArrRef v _  -> getArrayType v
    EBkt expr    -> inferType expr
    ECall f args -> getIdType f IdFunction

-- |Finds the type of an identifier. If it's not found or isn't of the kind
-- IdVariable, errors are logged and a default type is returned so type checking
-- can continue.
getIdType :: String -> IdentifierType -> MParser Type
getIdType var expected = do
  v <- findGlobalIdentifier var
  case v of
    Nothing -> (logError . UnknownIdentifierError $ var) >> return Number
    (Just e) ->
      if idType e == expected
      then return . fromJust . returnType $ e
      else (logError . InvalidIdKindError $ var ++ " is of kind " ++
           show (idType e) ++ ", expected " ++ show expected) >> return Number

-- |Finds the item type of an array. If the identifier doesn't refer to an array
-- , an error is logged.
getArrayType :: String -> MParser Type
getArrayType var = do
  t <- getIdType var IdVariable
  case t of
    (RefType t') -> return t'
    _            -> (logError . TypeError $
                    "expected reference (array) type, got " ++ show t)
                    >> return t

-- |Checks that a function call is to a function identifier and has the right
-- number and types of arguments.
checkFuncCall :: String -> ActualParams -> MParser ()
checkFuncCall = checkCall IdFunction

-- |Checks that a procedure call is to a procedure identifier and has the right
-- number and types of arguments.
checkProcCall :: String -> ActualParams -> MParser ()
checkProcCall = checkCall IdProcedure

-- |Helper function to type-check method calls.
checkCall :: IdentifierType -> String -> ActualParams -> MParser ()
checkCall idtype f args =
  withIdKindCheck idtype f $ \t -> checkArgs args t

-- |Checks whether an identifier is of a specified kind. Also checks whether the
--  identifier exists by means of 'withIdExistenceCheck'.
withIdKindCheck :: IdentifierType -> String ->
                   (SymbolTableEntry -> MParser ()) -> MParser ()
withIdKindCheck idtype f success =
  withIdExistenceCheck f $ \e ->
  if idType e == idtype
  then success e
  else logError . InvalidIdKindError $ f ++ " is of kind "
       ++ (show . idType) e ++  ", expected " ++ show idtype

-- |Checks whether the identifier f exists. If it does, success is applied to
-- the corresponding 'SymbolTableEntry'. Otherwise, an error is logged.
withIdExistenceCheck :: String -> (SymbolTableEntry -> MParser ()) -> MParser ()
withIdExistenceCheck f success = do
  f' <- findGlobalIdentifier f
  case f' of
    Nothing -> logError . UnknownIdentifierError $ f
    Just e -> success e

-- |Checks an assignment between two expressions for validity.
checkAssignment :: Expr -> Expr -> MParser ()
checkAssignment e1 e2 = do
  t1 <- inferType e1
  t2 <- inferType e2
  case testAssignOp t1 t2 of
    Right _  -> return ()
    Left err -> logError . TypeError $ err

-- |Checks whether the argument list given for a method matches its definition.
-- This function is only called with 'withIdKindCheck', so it doesn't have to
-- do any further safety checks.
checkArgs :: ActualParams -> SymbolTableEntry -> MParser ()
checkArgs aps f = do
  ps <- inferAParamTypes aps
  let ts = argumentTypes f
  if (and $ zipWith (==) ts ps)
    then return ()
    else logError . CallTypeError $
         (idString f) ++ " expects " ++ (unwords . map show $ ts) ++
         ", got " ++ (unwords . map show $ ps)

-- |Checks whether the given expression can be read into.
checkInput :: Expr -> MParser ()
checkInput (EId var) = do
  v <- getIdType var IdVariable
  case v of
    (RefType _) -> logError . TypeError $ "can't read in to reference type"
    _ -> return ()
checkInput (EArrRef arr _) = do
  atype <- getArrayType arr
  case atype of
    (RefType _) -> logError . TypeError $ "can't read in to reference type"
    _ -> return ()
checkInput _ =
  logError . TypeError $ "can only read in to variables or array elements"

-- |Returns a list of the types of actual parameters
-- (i.e. a list of expressions)
inferAParamTypes :: ActualParams -> MParser [Type]
inferAParamTypes (APList aps) =
  mapM inferType aps

-- |Returns a list of the types of formal parameters
-- (i.e. a list of <type><identifier> lexemes)
inferFParamTypes :: FormalParams -> MParser [Type]
inferFParamTypes (FPList fps) =
  return $ map getTypeFromFP fps
  where
    getTypeFromFP (Param t _) = t

-- |Helper function used to type-check unary expressions.
-- To be used in conjunction with any of the testU##### functions.
inferUnary :: (Type -> TestResult) -> Expr -> MParser Type
inferUnary test e1 = do
  t1 <- inferType e1
  case test t1 of
    Right t  -> return t
    Left msg -> (logError . TypeError $ msg) >> return t1

-- |Helper function used to type-check binary expressions.
-- To be used in conjunction with any of the testB##### functions.
inferBinary :: (Type -> Type -> TestResult) -> Expr -> Expr -> MParser Type
inferBinary test e1 e2 = do
  t1 <- inferType e1
  t2 <- inferType e2
  case (test t1 t2) of
    Right t  -> return t
    Left msg -> (logError . TypeError $ msg) >> return t1

-- |Checks if an expression has the expected type, if not an error is logged.
checkExpr :: Type -> Expr -> MParser ()
checkExpr expected expr = do
  actual <- inferType expr
  if expected == actual
    then return ()
    else logError . TypeError $
         "expected " ++ show expected ++ ", got " ++ show actual

-- |Checks whether the given expression can be returned in the current context,
-- i.e. whether the expression is inside a function of the same type.
checkReturnType :: Expr -> MParser ()
checkReturnType e = do
  f <- getCurrentScope
  f' <- findGlobalIdentifier f
  t <- inferType e
  case f' of
    Nothing -> logError . InvalidReturnError $ "outside of function body"
    Just e  ->
      case idType e of
        IdFunction ->
          if t == (fromJust . returnType $ e)
          then return ()
          else logError . InvalidReturnError $ "type mismatch, expected " ++
               show (returnType e) ++ ", got " ++ show t
        _           ->
          logError . InvalidReturnError $ "inside a procedure"

-- |Adds the method identifier to the current symbol table, creates a new
-- symbol table for the method and puts the method's arguments in it.
enterMethod :: String -> Maybe Type -> IdentifierType ->
               FormalParams -> MParser ()
enterMethod f rtype idtype fps@(FPList fpl) = do
  argtypes <- inferFParamTypes fps
  insertSymbol f rtype idtype argtypes
  newSymbolTable f
  mapM_ (\(Param t var) -> insertSymbol var (Just t) IdVariable []) fpl

-- |Removes the local symbol table as all local variables go out of scope.
exitMethod :: MParser ()
exitMethod = removeSymbolTable

-- |Enters a new code block by adding a symbol table with an empty scope name.
-- This is only used for blocks inside functions, and prevents them from
-- returning values.
enterBlock :: MParser ()
enterBlock = newSymbolTable ""

-- |Exits a local block.
exitBlock :: MParser ()
exitBlock = removeSymbolTable

-- |Checks whether the program has a valid entry point,
-- i.e. a procedure 'hatta'.
checkEntryPoint :: MParser ()
checkEntryPoint = do
  mainFunc <- findGlobalIdentifier "hatta"
  case mainFunc of
    Nothing -> logError . EntryPointError $ "no procedure 'hatta' found"
    Just e ->
      if idType e == IdProcedure
      then return ()
      else logError . EntryPointError $ "'hatta' is declared as a " ++
           show (idType e) ++ ", not a procedure"
