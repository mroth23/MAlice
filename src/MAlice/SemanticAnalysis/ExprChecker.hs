module MAlice.SemanticAnalysis.ExprChecker
       ( checkExpr
       , checkExpr_
       , inferType
       , inferTypeP
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
import MAlice.Parser.ParserState
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
  else fail "numeric, matching types on binary numeric operation" (t1, t2)

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

isBNumOp :: String -> Bool
isBNumOp = flip elem ["+", "-", "*", "/", "%", "|", "&", "^"]

isBBoolOp :: String -> Bool
isBBoolOp = flip elem ["||", "&&"]

isRelOp :: String -> Bool
isRelOp = flip elem [">", ">=", "<=", "<"]

isEqOp :: String -> Bool
isEqOp = flip elem ["==", "!="]

isUBoolOp :: String -> Bool
isUBoolOp = (==) "!"

isUNumOp = flip elem ["+", "-", "~"]

-- |Infers the type of an expression. This returns Maybe Type so the type
-- checker can stop checking expressions with known type errors in them.
-- Since procedures are specified as returning Nothing, expressions with
-- procedure calls automatically fail.
inferType :: Expr -> MParser (Maybe Type)
inferType ex@(EBinOp op e1 e2)
  | isBNumOp  op = inferBinary testBNumOp  e1 e2 ex
  | isBBoolOp op = inferBinary testBBoolOp e1 e2 ex
  | isRelOp   op = inferBinary testRelOp   e1 e2 ex
  | isEqOp    op = inferBinary testEqOp    e1 e2 ex
inferType ex@(EUnOp op e1)
  | isUBoolOp op = inferUnary testUBoolOp e1 ex
  | isUNumOp  op = inferUnary testUNumOp  e1 ex
inferType ex =
  case ex of
    EId t _        -> return t
    EString _      -> return (Just Sentence)
    EInt _         -> return (Just Number)
    EChar _        -> return (Just Letter)
    EBool _        -> return (Just Boolean)
    EArrRef t v _  -> return t
    ECall t _ _    -> return t

-- A pure version of inferType, that doesn't report errors. This assumes that
-- the program type checks correctly, and must only be used after the parsing
-- stage. Used e.g. in the code generator for JVM to infer types of expressions.
inferTypeP :: Expr -> Maybe Type
inferTypeP ex@(EBinOp op e1 e2)
  | isBNumOp  op = inferTypeP e1
  | isBBoolOp op = inferTypeP e1
  | isRelOp   op = Just Boolean
  | isEqOp    op = Just Boolean
inferTypeP ex@(EUnOp op e1)
  | isUBoolOp op = Just Boolean
  | isUNumOp  op = inferTypeP e1
inferTypeP ex =
  case ex of
    EId t _        -> t
    EString _      -> Just Sentence
    EInt _         -> Just Number
    EChar _        -> Just Letter
    EBool _        -> Just Boolean
    EArrRef t v _  -> t
    ECall t _ _    -> t

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

checkExpr_ :: Expr -> MParser ()
checkExpr_ expr = do
  _ <- inferType expr
  return ()
