{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module MAlice.Parsing.ParserState where

import MAlice.Language.Types
import MAlice.Language.SymbolTable
import Text.ParserCombinators.Parsec.Prim (GenParser (..), getPosition)
import Text.Parsec.Pos (SourcePos(..))
import Text.ParserCombinators.Parsec (getState, updateState)
import Control.Monad (liftM)

type MParser a = GenParser Char ParserState a

data ParserState =
  ParserState { errorList :: SemanticErrors
              , symTables :: [SymbolTable]
              , scopes :: [String] }

initState :: ParserState
initState =
  ParserState { errorList = SemanticErrors { errors = [] }
              , symTables = [[]]
              , scopes = [] }

newtype SemanticErrors =
  SemanticErrors { errors :: [(SemanticError, SourcePos)] }

instance Show SemanticErrors where
  show =
    concatMap (\(err, pos) -> show pos ++ ", " ++ show err ++ "\n") . errors

data SemanticError =
  TypeError                String |
  MultipleDeclarationError String |
  UnknownIdentifierError   String |
  CallTypeError            String |
  InvalidIdKindError       String |
  InvalidReturnError       String |
  EntryPointError          String

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

logError :: SemanticError -> MParser ()
logError perr = do
  st <- getState
  pos <- getPosition
  let el = errorList st
  let newEl = el { errors = errors el ++ [(perr, pos)]}
  updateState $ \st -> st { errorList = newEl }

newSymbolTable :: String -> MParser ()
newSymbolTable scope =
  updateState $ \st -> st { symTables = [] : symTables st
                          , scopes = scope : scopes st }

removeSymbolTable :: MParser ()
removeSymbolTable =
  updateState $ \st -> st { symTables = tail . symTables $ st
                          , scopes = tail . scopes $ st }

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

-- |Returns the name of the current scope
getCurrentScope :: MParser String
getCurrentScope =
  head `liftM` (scopes `liftM` getState)