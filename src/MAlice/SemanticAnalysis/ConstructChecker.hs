module MAlice.SemanticAnalysis.ConstructChecker
       ( enterMethod
       , exitMethod
       , enterBlock
       , exitBlock
       , checkEntryPoint
       , checkReturnPath
       ) where

import MAlice.Language.AST
import MAlice.Language.SymbolTable
import MAlice.Language.Types
import MAlice.Parsing.ParserState

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
enterBlock = getCurrentScope >>= newSymbolTable

-- |Exits a local block.
exitBlock :: MParser ()
exitBlock = removeSymbolTable

-- |Returns a list of the types of formal parameters
-- (i.e. a list of <type><identifier> lexemes)
inferFParamTypes :: FormalParams -> MParser [Type]
inferFParamTypes (FPList fps) =
  return $ map getTypeFromFP fps
  where
    getTypeFromFP (Param t _) = t

-- |Checks whether the program has a valid entry point,
-- i.e. a procedure 'hatta'.
checkEntryPoint :: MParser ()
checkEntryPoint = do
  setContext ""
  mainFunc <- findGlobalIdentifier "hatta"
  case mainFunc of
    Nothing -> logError . EntryPointError $ "no procedure 'hatta' found"
    Just e ->
      if idType e == IdProcedure
      then return ()
      else logError . EntryPointError $ "'hatta' is declared as a " ++
           show (idType e) ++ ", not a procedure"

-- |Checks whether all paths through the function return a value.
-- If there is a top-level return, the function will always return.
-- Otherwise, if there is a conditional or block that is guaranteed
-- to return a value, the function will also return.
checkReturnPath :: Body -> String -> MParser ()
checkReturnPath (StmtBody cst) func =
  if cReturns cst
  then return ()
  else logWarning . FunctionReturnPathWarning $ func
checkReturnPath (DeclBody _ cst) func =
  if cReturns cst
  then return ()
  else logWarning . FunctionReturnPathWarning $ func
checkReturnPath EmptyBody func =
  logWarning . EmptyFunctionWarning $ func

cReturns :: CompoundStmt -> Bool
cReturns (CSList csl) =
  topLevelReturns || conditionalReturns
  where
    topLevelReturns    = hasTopLevelReturns csl
    conditionalReturns = hasConditionalReturns csl

hasTopLevelReturns :: [Stmt] -> Bool
hasTopLevelReturns [] =
  False
hasTopLevelReturns (s : ss) =
  case s of
    SReturn _ -> True
    _         -> hasTopLevelReturns ss

hasConditionalReturns :: [Stmt] -> Bool
hasConditionalReturns [] =
  False
hasConditionalReturns (s : ss) =
  case s of
    SBody (StmtBody cst) ->
      (cReturns cst) || hasConditionalReturns ss
    SBody (DeclBody _ cst) ->
      (cReturns cst) || hasConditionalReturns ss
    SIf ifs ->
      ifReturns ifs || hasConditionalReturns ss
    _ ->
      hasConditionalReturns ss

ifReturns :: [IfClause] -> Bool
ifReturns ifs =
  case last ifs of
    Else _ -> all clauseReturns ifs
    _      -> ifReturns $ ifs ++ [Else (CSList [SNull])]

clauseReturns :: IfClause -> Bool
clauseReturns (If _ cst) =
  cReturns cst
clauseReturns (Else cst) =
  cReturns cst
