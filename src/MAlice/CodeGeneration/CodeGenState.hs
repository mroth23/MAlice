module MAlice.CodeGeneration.CodeGenState where

import MAlice.Language.SymbolTable
import MAlice.Parsing.ParserState

newtype CodeGenState = CodeGenState { symbolTable :: [SymbolTable] }

initState st = CodeGenState { symbolTable = symTables st }