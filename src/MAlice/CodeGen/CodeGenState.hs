module MAlice.CodeGen.CodeGenState where

import MAlice.Language.SymbolTable
import MAlice.Parser.ParserState

newtype CodeGenState = CodeGenState { symbolTable :: [SymbolTable] }

initState st = CodeGenState { symbolTable = [] }