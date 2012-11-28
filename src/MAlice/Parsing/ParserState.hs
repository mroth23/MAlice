module MAlice.Parsing.ParserState where

import MAlice.Language.Types
import MAlice.Language.SymbolTable
import Text.Parsec.Prim (Parsec (..), getPosition, getInput)
import Text.Parsec.Pos (SourcePos(..))
import Text.Parsec (getState, updateState)
import Control.Monad (liftM)

-- |A standard Parser from Text.Parsec.Prim with our custom parser state
type MParser a = Parsec String ParserState a

-- |The state passed around by the parser
data ParserState =
  ParserState { errorList :: SemanticErrors
              , warnList :: SemanticWarnings
              , symTables :: [SymbolTable]
              , scopes :: [String] }

-- |The initial parser state with no errors, an empty global symbol table
-- and no defined scopes
initState :: ParserState
initState =
  ParserState { errorList = SemanticErrors { errors = [] }
              , warnList = SemanticWarnings { warnings = [] }
              , symTables = [[]]
              , scopes = [] }

-- |Data type used in 'ParserState' to store semantic errors
newtype SemanticErrors =
  SemanticErrors { errors :: [(SemanticError, SourcePos, String)] }

-- This is newtype so we can make a show instance
-- without language extensions
instance Show SemanticErrors where
  -- Prints all errors contained in the data type line by line
  show =
    concatMap (\(err, pos, inp) ->
                "Semantic error in " ++ show pos
                ++ ":\n"++ (head . lines $ inp) ++ "\n" ++ show err ++ "\n") .
    errors

-- |Data type used in 'ParserState' to store semantic warnings
newtype SemanticWarnings =
  SemanticWarnings { warnings :: [(SemanticWarning, SourcePos, String)] }

instance Show SemanticWarnings where
  -- Prints all warnings contained in the data type line by line
  show =
    concatMap (\(wrn, pos, inp) ->
                "Warning in " ++ show pos
                ++ ":\n" ++ show wrn ++ "\n") .
    warnings

-- |The different kinds of semantic errors that can occur, each has space for
-- a custom message String
data SemanticError =
  TypeError                String |
  MultipleDeclarationError String |
  UnknownIdentifierError   String |
  CallTypeError            String |
  InvalidIdKindError       String |
  InvalidReturnError       String |
  EntryPointError          String

-- How the errors are shown by the compiler
instance Show SemanticError where
  show (TypeError msg) =
    "Type error: " ++ msg
  show (MultipleDeclarationError msg) =
    "Identifier already in use: " ++ msg
  show (UnknownIdentifierError msg) =
    "Identifier not in scope: " ++ msg
  show (CallTypeError msg) =
    "Call signature mismatch: " ++ msg
  show (InvalidIdKindError msg) =
    "Invalid identifier kind: " ++ msg
  show (InvalidReturnError msg) =
    "Invalid return statement: " ++ msg
  show (EntryPointError msg) =
    "Program entry point error: " ++ msg

data SemanticWarning =
  FunctionReturnPathWarning String |
  EmptyFunctionWarning      String |
  IdInitialisationWarning   String

instance Show SemanticWarning where
  show (FunctionReturnPathWarning ident) =
    "Function " ++ ident ++ " might not return a value on all code paths"
  show (EmptyFunctionWarning ident) =
    "Function " ++ ident ++ " is empty and calling it may cause runtime errors"
  show (IdInitialisationWarning ident) =
    "Variable " ++ ident ++ " might not be initialised on all code paths" ++
    ", this may cause a runtime error"

-- |Logs an error to the parser state together with the current position
logError :: SemanticError -> MParser ()
logError perr = do
  st <- getState
  pos <- getPosition
  inp <- getInput
  let el = errorList st
  let newEl = el { errors = errors el ++ [(perr, pos, inp)]}
  updateState $ \st -> st { errorList = newEl }

-- |Logs a warning to the parser state together with the current position
logWarning :: SemanticWarning -> MParser ()
logWarning perr = do
  st <- getState
  pos <- getPosition
  inp <- getInput
  let el = warnList st
  let newEl = el { warnings = warnings el ++ [(perr, pos, inp)]}
  updateState $ \st -> st { warnList = newEl }

-- |Adds a new symbol table for a scope with a given name
newSymbolTable :: String -> MParser ()
newSymbolTable scope =
  updateState $ \st -> st { symTables = [] : symTables st
                          , scopes = scope : scopes st }

-- |Removes the "newest" symbol table in the hierarchy
removeSymbolTable :: MParser ()
removeSymbolTable =
  updateState $ \st -> st { symTables = tail . symTables $ st
                          , scopes = tail . scopes $ st }

-- |Pulls the symbol table hierarchy (stack) out of the parser state
getSymbolTables :: MParser [SymbolTable]
getSymbolTables = symTables `liftM` getState

-- |Finds an identifier defined in any (reachable) scope.
findGlobalIdentifier :: String -> MParser (Maybe SymbolTableEntry)
findGlobalIdentifier v =
  (lookupInTables v) `liftM` getSymbolTables

-- |Finds an identifier defined in the current scope. This is used in
-- 'checkLocalIdentifier' to check for double declarations in the same scope.
findLocalIdentifier :: String -> MParser (Maybe SymbolTableEntry)
findLocalIdentifier v =
  ((lookupInTable v) . head) `liftM` getSymbolTables

-- |Checks whether the given identifier is already defined in the current scope.
checkLocalIdentifier :: String -> MParser ()
checkLocalIdentifier ident = do
  currentDecl <- findLocalIdentifier ident
  case currentDecl of
    (Just ste) -> logError . MultipleDeclarationError $ ident
    _          -> return ()

-- |Inserts a symbol into the symbol table
insertSymbol :: String -> Maybe Type -> IdentifierType -> ArgTypes -> MParser ()
insertSymbol ident vartype idtype argtypes = do
  checkLocalIdentifier ident
  (t:ts) <- getSymbolTables
  let newt = addSymbol ident vartype idtype argtypes t
  updateState $ \st -> st { symTables = newt : ts }

-- |Returns the name of the current scope. This is used to lookup the current
-- function for return type checking.
getCurrentScope :: MParser String
getCurrentScope =
  head `liftM` (scopes `liftM` getState)