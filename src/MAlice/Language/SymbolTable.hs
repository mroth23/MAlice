module MAlice.Language.SymbolTable where
import MAlice.Language.AST (Type(..))

type SymbolTable = [SymbolTableEntry]

data SymbolTableEntry = SymbolTableEntry
                        { idString :: String
                        , returnType :: Type
                        , argumentTypes :: ArgTypes }

type ArgTypes = [Type]

lookupSymbol :: String -> SymbolTable -> SymbolTableEntry
lookupSymbol _ [] = error "Not In Table"
lookupSymbol symbol (entry:rest)
  | idString entry  == symbol = entry
  | otherwise                 = lookupSymbol symbol rest
  

existsInTable :: String -> SymbolTable -> Bool
existsInTable symbol [] = False
existsInTable symbol (entry:rest)
  | idString entry == symbol = True
  | otherwise                = existsInTable symbol rest

addSymbol :: String -> Type -> ArgTypes -> SymbolTable -> SymbolTable
addSymbol ident vtype argtypes table
  | existsInTable ident table = table
  | otherwise                 = table ++ [SymbolTableEntry{idString = ident, returnType = vtype, argumentTypes = argtypes}]
