module MAlice.Language.SymbolTable where

import MAlice.Language.Types (Type(..))
import Control.Monad

type SymbolTable = [SymbolTableEntry]

data SymbolTableEntry =
  SymbolTableEntry
  { idString :: String         -- String identifier
  , idType :: IdentifierType   -- Identifier type
  , returnType :: Maybe Type   -- Return type / variable type
  , argumentTypes :: ArgTypes }-- Argument types (if any)

data IdentifierType =
  IdVariable | IdFunction | IdProcedure
  deriving (Eq)

instance Show IdentifierType where
  show IdVariable = "Variable"
  show IdFunction = "Function"
  show IdProcedure = "Procedure"

type ArgTypes = [Type]

lookupInTables :: String -> [SymbolTable] -> Maybe SymbolTableEntry
lookupInTables symbol [] =
  Nothing
lookupInTables symbol (t:ts) =
  case lookupInTable symbol t of
    Nothing -> lookupInTables symbol ts
    Just e  -> Just e

lookupInTable :: String -> SymbolTable -> Maybe SymbolTableEntry
lookupInTable _ [] = Nothing
lookupInTable symbol (entry:rest)
  | idString entry == symbol = Just entry
  | otherwise               = lookupInTable symbol rest

existsInTable :: String -> SymbolTable -> Bool
existsInTable symbol [] = False
existsInTable symbol (entry:rest)
  | idString entry == symbol = True
  | otherwise               = existsInTable symbol rest

addSymbol :: String -> Maybe Type -> IdentifierType -> ArgTypes ->
             SymbolTable -> SymbolTable
addSymbol ident vtype idtype argtypes table
  | existsInTable ident table = table
  | otherwise =
      table ++ [SymbolTableEntry { idString = ident
                                 , idType = idtype
                                 , returnType = vtype
                                 , argumentTypes = argtypes }]
