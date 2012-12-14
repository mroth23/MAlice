module MAlice.Parser.ParserState
       ( MParser
       , ParserState(..)
       , initState
       , SemanticErrors(..)
       , SemanticError(..)
       , SemanticWarnings(..)
       , SemanticWarning(..)
       , logError
       , logWarning
       , newSymbolTable
       , removeSymbolTable
       , findGlobalIdentifier
       , checkLocalIdentifier
       , insertSymbol
       , getCurrentScope
       , getContext
       , setContext
       , recordPosition
       , clearPosition )
where

import MAlice.Language.Types
import MAlice.Language.SymbolTable
import Text.Parsec.Prim (Parsec, getPosition)
import Text.Parsec.Pos (SourcePos)
import Text.Parsec (getState, updateState)
import Control.Monad (liftM)

-- This module exists to hide most of the functionality of the parser from the
-- type checker and other classes. They have methods to access and modify state,
-- log errors and get some type constructors / synonyms. The actual
-- implementation of e.g. the symbol table is unimportant and can be changed,
-- without the need to modify other code.

-- |A standard Parser from Text.Parsec.Prim with our custom parser state
type MParser a = Parsec String ParserState a

-- |The state passed around by the parser
data ParserState =
  ParserState { errorList :: SemanticErrors
              , warnList :: SemanticWarnings
              , symTables :: [SymbolTable]
              , scopes :: [String]
              , context :: String
              , customPos :: [SourcePos] }

-- |The initial parser state with no errors, an empty global symbol table
-- and no defined scopes
initState :: ParserState
initState =
  ParserState { errorList = SemanticErrors { errors = [] }
              , warnList = SemanticWarnings { warnings = [] }
              , symTables = [[]]
              , scopes = []
              , context = ""
              , customPos = [] }

-- |Data type used in 'ParserState' to store semantic errors
newtype SemanticErrors =
  SemanticErrors { errors :: [(SemanticError, SourcePos, String)] }

-- This is newtype so we can make a show instance
-- without language extensions
instance Show SemanticErrors where
  -- Prints all errors contained in the data type line by line
  show =
    concatMap (
      \(err, pos, inp) ->
      "Semantic error in " ++ show pos ++ ":\n>  "++
       inp ++ "\n" ++ show err ++ "\n\n") .
    errors

-- |Data type used in 'ParserState' to store semantic warnings
newtype SemanticWarnings =
  SemanticWarnings { warnings :: [(SemanticWarning, SourcePos, String)] }

instance Show SemanticWarnings where
  -- Prints all warnings contained in the data type line by line
  show =
    concatMap (\(wrn, pos, _) ->
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
  EmptyFunctionWarning      String

instance Show SemanticWarning where
  show (FunctionReturnPathWarning ident) =
    "Function " ++ ident ++ " might not return a value on all code paths"
  show (EmptyFunctionWarning ident) =
    "Function " ++ ident ++ " is empty and calling it may cause runtime errors"

-- |Logs an error to the parser state together with the current position
logError :: SemanticError -> MParser ()
logError perr = do
  st  <- getState
  pos <- getCPos
  inp <- getContext
  let el    = errorList st
      newEl = el { errors = errors el ++ [(perr, pos, inp)]}
  updateState $ \st' -> st' { errorList = newEl }

-- |Logs a warning to the parser state together with the current position
logWarning :: SemanticWarning -> MParser ()
logWarning perr = do
  st  <- getState
  pos <- getCPos
  inp <- getContext
  let el    = warnList st
      newEl = el { warnings = warnings el ++ [(perr, pos, inp)]}
  updateState $ \st' -> st' { warnList = newEl }

-- |Adds a new symbol table for a scope with a given name
newSymbolTable :: String -> MParser ()
newSymbolTable scope =
  updateState $ \st -> st { symTables = [] : symTables st
                          , scopes   = scope : scopes st }

-- |Removes the "newest" symbol table in the hierarchy
removeSymbolTable :: MParser ()
removeSymbolTable =
  updateState $ \st -> st { symTables = tail . symTables $ st
                         , scopes    = tail . scopes $ st }

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
    (Just _) -> logError . MultipleDeclarationError $ ident
    _        -> return ()

-- |Inserts a symbol into the symbol table
insertSymbol :: String -> Type -> IdentifierType -> ArgTypes -> MParser ()
insertSymbol ident vartype idtype argtypes = do
  setContext $ "Declaration of '"++ident ++ "' with type "++ (show vartype)
  checkLocalIdentifier ident
  (t : ts) <- getSymbolTables
  let newt = addSymbol ident vartype idtype argtypes t
  updateState $ \st -> st { symTables = newt : ts }

-- |Returns the name of the current scope. This is used to lookup the current
-- function for return type checking.
getCurrentScope :: MParser String
getCurrentScope =
  head `liftM` (scopes `liftM` getState)

-- |Context string handling. This is used for error output. The context is a
-- string description (sometimes in the form of PARSED code) of how the compiler
-- understands the code which caused the error. This is not always exactly
-- equivalent to the source code, and may have additional explanations.
getContext :: MParser String
getContext =
  context `liftM` getState

setContext :: String -> MParser ()
setContext nc =
  updateState $ \st -> st { context = nc }

-- |Allows to record a custom position used in error messages. For example, type
-- checking is usually done after a statement / expression is fully parsed, so
-- that parse errors show up before type errors. But the source position shown
-- in the error message will then be after the statement (sometimes even on a
-- new line). This allows us to circumvent this limitation.
getCPos :: MParser (SourcePos)
getCPos = do
  st <- getState
  case customPos st of
    [] -> getPosition
    (p : _) -> return p

-- |Records the current position for later use.
recordPosition :: MParser ()
recordPosition = do
  st <- getState
  curr <- getPosition
  setCPos $ curr : (customPos st)

clearPosition :: MParser ()
clearPosition = do
  st <- getState
  case customPos st of
    []       -> return ()
    (_ : ps) -> setCPos ps

setCPos :: [SourcePos] -> MParser ()
setCPos ps =
  updateState $ \st -> st { customPos = ps }