module MAlice.Parsing.ParserState where

import MAlice.Language.Types
import MAlice.Language.SymbolTable
import Text.ParserCombinators.Parsec.Prim (GenParser (..))
import Text.ParserCombinators.Parsec (getState, updateState)
import Control.Monad (liftM)

type MParser a = GenParser Char ParserState a

data ParserState =
  ParserState { errorList :: [SemanticError]
              , symTables :: [SymbolTable] }

initState :: ParserState
initState =
  ParserState { errorList = []
              , symTables = [[]] }

data SemanticError =
  TypeError                String |
  MultipleDeclarationError String |
  UnknownIdentifierError   String |
  CallTypeError            String |
  InvalidCalleeError       String

instance Show SemanticError where
  show (TypeError msg) =
    "Type error: " ++ msg
  show (MultipleDeclarationError msg) =
    "Identifier already in use: " ++ msg
  show (UnknownIdentifierError msg) =
    "Identifier not in scope: " ++ msg
  show (CallTypeError msg) =
    "Call signature mismatch: " ++ msg
  show (InvalidCalleeError msg) =
    "Invalid callee: " ++ msg

logError :: SemanticError -> MParser ()
logError perr =
  updateState $ \st -> st { errorList = errorList st ++ [perr] }

newSymbolTable :: MParser ()
newSymbolTable =
  updateState $ \st -> st { symTables = [] : symTables st }

removeSymbolTable :: MParser ()
removeSymbolTable =
  updateState $ \st -> st { symTables = tail . symTables $ st }

getSymbolTables :: MParser [SymbolTable]
getSymbolTables = symTables `liftM` getState

findGlobalIdentifier :: String -> MParser (Maybe SymbolTableEntry)
findGlobalIdentifier v =
  (lookupInTables v) `liftM` getSymbolTables

findLocalIdentifier :: String -> MParser (Maybe SymbolTableEntry)
findLocalIdentifier v =
  ((lookupInTable v) . head) `liftM` getSymbolTables

checkLocalIdentifier :: String -> MParser ()
checkLocalIdentifier ident = do
  currentDecl <- findLocalIdentifier ident
  case currentDecl of
    (Just ste) -> logError . MultipleDeclarationError $ ident
    _          -> return ()

insertSymbol :: String -> Maybe Type -> IdentifierType -> ArgTypes -> MParser ()
insertSymbol ident vartype idtype argtypes = do
  checkLocalIdentifier ident
  (t:ts) <- getSymbolTables
  let newt = addSymbol ident vartype idtype argtypes t
  updateState $ \st -> st { symTables = newt : ts }