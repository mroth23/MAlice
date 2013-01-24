module MAlice.CodeGen.JavaBytecodeSetupInput where

-- This module is used to add the setup for input
-- in the code. If it's not need we may as well
-- not add it, as it has to be run in the constructor
-- and have global access.

import MAlice.CodeGen.JavaBytecodeInstr
import MAlice.CodeGen.JavaBytecodeUtil

-- Returns true if the program uses input.
usesInput :: JProgram -> Bool
usesInput []
  = False
usesInput ((Getfield meth "Ljava/util/Scanner;"):_)
  = True
    where
      meth = getClassName++"/_scanner"
usesInput (_:rest)
  = usesInput rest

-- The input code uses a field and a constructor to set it up.
inputConstructor :: JProgram
inputConstructor
  = [Field "_scanner" "Ljava/util/Scanner;"] ++
    [Constructor
     ([ALoad_0]                                                             ++
     [New "java/util/Scanner"]                                              ++
     [Dup]                                                                  ++
     [Getstatic "java/lang/System.in" "Ljava/io/InputStream;"]              ++
     [Invokespecial "java/util/Scanner/<init>" "Ljava/io/InputStream;" "V"] ++
     [Putfield (getClassName++"/_scanner") "Ljava/util/Scanner;"])]

-- Adds input code, only if required.
setupInputIfRequired :: JProgram -> JProgram
setupInputIfRequired program
  | usesInput program = inputConstructor ++ program
  | otherwise         = program
