module MAlice.CodeGen.JavaBytecodeThrowable where

-- Module that will add our runtime throw erro
-- for when no return is found and the end
-- of the function is hit.

import MAlice.CodeGen.JavaBytecodeInstr
import MAlice.CodeGen.JavaBytecodeUtil

-- Add the function if the program can throw
-- and error.
setupThrowableIfRequired :: JProgram -> JProgram
setupThrowableIfRequired program
  | usesThrowable program  = program++[ThrowConditionError]
  | otherwise              = program

-- Returns true if the program can throw the error.
usesThrowable :: JProgram -> Bool
usesThrowable []
  = False
usesThrowable ((Invokevirtual call "" "V"):_)
  = True
    where
      call = (getClassName++"/"++"_throwConditionError")
usesThrowable (_:rest)
  = usesThrowable rest
