module MAlice.Language.SymbolTable where

import MAlice.Language.Types (Type(..))
import MAlice.Language.AST (Expr)
import Control.Monad

type SymbolTable = [SymbolTableEntry]

-- |A symbol table entry, has all potentially useful information about
-- identifiers. 'returnType' is Nothing for procedures, argumentTypes is
-- empty list for variables. In a way, they can be regarded as functions
-- without arguments.
data SymbolTableEntry =
  SymbolTableEntry
  { idString       :: String         -- Identifier string (name)
  , idType         :: IdentifierType -- Identifier type
  , returnType     :: Type           -- Return type / variable type
  , argumentTypes  :: ArgTypes }     -- Argument types (if any)
  deriving (Eq, Show)

-- |The different kinds of identifiers: Functions, Procedures and Variables
data IdentifierType =
  IdVariable | IdFunction | IdProcedure
  deriving (Eq)

-- Print the kind slightly nicer in error messages
instance Show IdentifierType where
  show IdVariable = "Variable"
  show IdFunction = "Function"
  show IdProcedure = "Procedure"

type ArgTypes = [Type]

-- |Looks up an identifier in a symbol table hierarchy, with the most recent
-- definition returned first, or Nothing returned if the identifier is
-- undefined.
lookupInTables :: String -> [SymbolTable] -> Maybe SymbolTableEntry
lookupInTables symbol [] =
  Nothing
lookupInTables symbol (t:ts) =
  case lookupInTable symbol t of
    Nothing -> lookupInTables symbol ts
    Just e  -> Just e

-- |Looks up and identifier in the given symbol table.
lookupInTable :: String -> SymbolTable -> Maybe SymbolTableEntry
lookupInTable _ [] = Nothing
lookupInTable symbol (entry:rest)
  | idString entry == symbol = Just entry
  | otherwise               = lookupInTable symbol rest

-- |Checks whether an identifier is defined in the given symbol table.
existsInTable :: String -> SymbolTable -> Bool
existsInTable symbol [] = False
existsInTable symbol (entry:rest)
  | idString entry == symbol = True
  | otherwise               = existsInTable symbol rest

-- |Creates a new symbol table with the given symbol added to it. This is
-- used to update the state of the parser with new symbols.
addSymbol :: String -> Type -> IdentifierType -> ArgTypes ->
             SymbolTable -> SymbolTable
addSymbol ident vtype idtype argtypes table
  | existsInTable ident table = table
  | otherwise =
      table ++ [SymbolTableEntry { idString       = ident
                                 , idType         = idtype
                                 , returnType     = vtype
                                 , argumentTypes  = argtypes }]
