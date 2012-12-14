module MAlice.CodeGen.JavaBytecodeThrowable where

import MAlice.CodeGen.JavaBytecodeInstr
import MAlice.CodeGen.JavaBytecodeUtil

setupThrowableIfRequired :: JProgram -> JProgram
setupThrowableIfRequired program
  | usesThrowable program  = program++[ThrowConditionError]
  | otherwise              = program

usesThrowable :: JProgram -> Bool
usesThrowable []
  = False
usesThrowable ((Invokevirtual call "" "V"):_)
  = True
    where
      call = (thisClass++"/"++"_throwConditionError")
usesThrowable (_:rest)
  = usesThrowable rest
