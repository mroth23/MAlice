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

lookupSymbol :: String -> SymbolTable -> SymbolTableEntry
lookupSymbol _ [] = error "Not In Table"
lookupSymbol symbol (entry:rest)
  | idString entry  == symbol = entry
  | otherwise                = lookupSymbol symbol rest

existsInTable :: String -> SymbolTable -> Bool
existsInTable symbol [] = False
existsInTable symbol (entry:rest)
  | idString entry == symbol = True
  | otherwise                = existsInTable symbol rest

addSymbol :: String -> Type -> ArgTypes -> SymbolTable -> SymbolTable
addSymbol ident vtype argtypes table
  | existsInTable ident table = table
  | otherwise                 = table ++ [SymbolTableEntry{idString = ident, returnType = vtype, argumentTypes = argtypes}]
