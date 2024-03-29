module MAlice.SemanticAnalysis.StmtChecker
       ( checkFuncCall
       , checkProcCall
       , checkArgs
       , checkAssignment
       , checkInput
       , checkReturnType
       ) where

import MAlice.Language.AST
import MAlice.Language.SymbolTable
import MAlice.Language.Types
import MAlice.Language.Utilities
import MAlice.Parser.ParserState
import MAlice.SemanticAnalysis.ExprChecker

import Prelude hiding (fail)

-- |Check the type for an assignment operation. Reference types can't be
-- assigned to, all other types accept assignments from equal types.
testAssignOp :: Type -> Type -> TestResult
testAssignOp (RefType _) _ =
  Left "can't assign to a reference type (constant)"
testAssignOp t1 t2 =
  if (t1 == t2)
  then succeed t1
  else fail "two compatible types on assignment operation" (t1, t2)

-- |Checks that a function call is to a function identifier and has the right
-- number and types of arguments.
checkFuncCall :: String -> ActualParams -> MParser ()
checkFuncCall f as =
  checkCall IdFunction f as $ show (ECall Void f as)

-- |Checks that a procedure call is to a procedure identifier and has the right
-- number and types of arguments.
checkProcCall :: String -> ActualParams -> MParser ()
checkProcCall f as =
  checkCall IdProcedure f as $ show (ECall Void f as)

-- |Helper function to type-check method calls.
checkCall :: IdentifierType -> String -> ActualParams -> String -> MParser ()
checkCall idtype f args cxt = do
  setContext cxt
  withIdKindCheck idtype f $ \t -> checkArgs args t

-- |Checks whether the argument list given for a method matches its definition.
-- This function is only called with 'withIdKindCheck', so it doesn't have to
-- do any further safety checks.
checkArgs :: ActualParams -> SymbolTableEntry -> MParser ()
checkArgs aps f = do
  ps <- inferAParamTypes aps
  let ts = argumentTypes f
  if ts == ps
    then return ()
    else logError . CallTypeError $
         (idString f) ++ " expects " ++ (showTypes ts) ++
         ", got " ++ (showTypes ps)

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
checkAssignment e1@(EId t var) e2 = do
  withIdKindCheck IdVariable var $
    const (checkAssignment' t e1 e2)
checkAssignment e1@(EArrRef t var _) e2 = do
  withIdKindCheck IdVariable var $
    const (checkAssignment' t e1 e2)
checkAssignment e1 e2 = do
  setContext $ (show e1) ++ " became " ++ (show e2)
  logError . TypeError $
    "Values can only be assigned to variables and array elements"

checkAssignment' :: Type -> Expr -> Expr -> MParser ()
checkAssignment' t1 e1 e2 = do
  t2 <- inferType e2
  setContext $ (show e1) ++ " became " ++ (show e2)
  case (t1, t2) of
    (Invalid, _) -> return ()
    (_, Invalid) -> return ()
    (t1', t2')   ->
      case testAssignOp t1' t2' of
        Right _  -> return ()
        Left err -> logError . TypeError $ err

-- |Checks whether the given expression can be read into.
checkInput :: Expr -> MParser ()
checkInput e@(EId v _) = do
  setContext $ "what was " ++ (show e) ++ "?"
  case v of
    (RefType _) -> logError . TypeError $ "Can't read in to reference type"
    (Boolean)   -> logError . TypeError $ "Can't read in boolean values"
    _           -> return ()

checkInput (EArrRef atype _ _) = do
  case atype of
    (RefType _) -> logError . TypeError $ "Can't read in to reference type"
    _           -> return ()

checkInput _ =
  logError . TypeError $ "Can only read in to variables or array elements"

-- |Returns a list of the types of actual parameters
-- (i.e. a list of expressions)
inferAParamTypes :: ActualParams -> MParser [Type]
inferAParamTypes (APList aps) =
  mapM inferType aps

-- |Checks whether the given expression can be returned in the current context,
-- i.e. whether the expression is inside a function of the same type.
checkReturnType :: Expr -> MParser ()
checkReturnType e = do
  f  <- getCurrentScope
  f' <- findGlobalIdentifier f
  t  <- inferType e
  case f' of
    Nothing -> logError . InvalidReturnError $ "outside of function body"
    Just f''  ->
      case idType f'' of
        IdFunction ->
          if t == returnType f''
          then return ()
          else logError . InvalidReturnError $ "type mismatch, expected " ++
               show (returnType f'') ++ ", got " ++ show t
        _          ->
          logError . InvalidReturnError $ "inside a procedure"
