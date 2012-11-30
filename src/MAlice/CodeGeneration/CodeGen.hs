module MAlice.CodeGeneration.CodeGen where

import Control.Monad.State
import Control.Monad.Identity
import MAlice.CodeGeneration.CodeGenState as C
import MAlice.CodeGeneration.ASM
import MAlice.Parsing.ParserState as P
import MAlice.Language.AST

--Replace Identity by IO/whatever to get more functionality
type CodeGen a = StateT CodeGenState Identity a

runCodeGen :: (Program, ParserState) -> (Program -> CodeGen [Instruction]) ->
              Either String [Instruction]
runCodeGen (program, pst) generateCode =
  Right . runIdentity $ evalStateT (generateCode program) (C.initState pst)
