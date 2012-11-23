module MAlice.Parsing.ParserState where

import MAlice.Language.SymbolTable
import Text.ParserCombinators.Parsec.Prim (GenParser (..))
import Text.ParserCombinators.Parsec (getState, updateState)
import Control.Monad (liftM)

type MParser a = GenParser Char ParserState a

data ParserState = ParserState
                   { errorList :: [SemanticError]
                   , symTables :: [SymbolTable] }

getSymbolTables :: MParser [SymbolTable]
getSymbolTables = symTables `liftM` getState

initState :: ParserState
initState =
  ParserState { errorList = []
              , symTables = [] }

data SemanticError =
  TypeError                String |
  MultipleDeclarationError String |
  UnknownIdentifierError   String |
  CallTypeError            String

instance Show SemanticError where
  show (TypeError msg) =
    "Type error: " ++ msg
  show (MultipleDeclarationError msg) =
    "Identifier already in use: " ++ msg
  show (UnknownIdentifierError msg) =
    "Identifier not in scope: " ++ msg
  show (CallTypeError msg) =
    "Call signature mismatch: " ++ msg

logError :: SemanticError -> MParser ()
logError perr =
  updateState $ \st -> st { errorList = errorList st ++ [perr] }