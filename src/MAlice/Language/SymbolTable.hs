module MAlice.Language.SymbolTable where
import MAlice.Language.AST (Type(..))

type SymbolTable = [SymbolTableEntry]

data SymbolTableEntry = SymbolTableEntry
                        { idString :: String
                        , returnType :: Type
                        , argumentTypes :: ArgTypes }

type ArgTypes = [Type]

lookupSymbol :: String -> SymbolTable -> SymbolTableEntry
lookupSymbol = undefined

existsInTable :: String -> SymbolTable -> Bool
existsInTable = undefined

addSymbol :: String -> Type -> ArgTypes -> SymbolTable -> SymbolTable
addSymbol ident vtype argtypes = undefined