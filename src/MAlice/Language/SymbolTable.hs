module MAlice.Language.SymbolTable where
import MAlice.Language.Types (Type(..))

type SymbolTable = [SymbolTableEntry]

data SymbolTableEntry = SymbolTableEntry
                        { idString :: String
                        , idType :: IdentifierType
                        , returnType :: Type
                        , argumentTypes :: ArgTypes }

data IdentifierType = IdVariable | IdFunction | IdProcedure

type ArgTypes = [Type]

lookupInTables :: String -> [SymbolTable] -> Maybe SymbolTableEntry
lookupInTables symbol [] =
  Nothing
lookupInTables symbol (t:ts) =
  case lookupSymbol symbol t of
    Nothing -> lookupInTables symbol ts
    Just e  -> Just e

lookupSymbol :: String -> SymbolTable -> Maybe SymbolTableEntry
lookupSymbol _ [] = Nothing
lookupSymbol symbol (entry:rest)
  | idString entry == symbol = Just entry
  | otherwise               = lookupSymbol symbol rest

existsInTable :: String -> SymbolTable -> Bool
existsInTable symbol [] = False
existsInTable symbol (entry:rest)
  | idString entry == symbol = True
  | otherwise               = existsInTable symbol rest

addSymbol :: String -> Type -> ArgTypes -> SymbolTable -> SymbolTable
addSymbol ident vtype argtypes table
  | existsInTable ident table = table
  | otherwise                 = table ++ [SymbolTableEntry{idString = ident, returnType = vtype, argumentTypes = argtypes}]
