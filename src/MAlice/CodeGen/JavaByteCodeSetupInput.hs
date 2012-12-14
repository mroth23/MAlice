module MAlice.CodeGen.JavaByteCodeSetupInput where

import MAlice.CodeGen.JavaByteCodeInstr
import MAlice.CodeGen.JavaByteCodeUtil

usesInput :: JProgram -> Bool
usesInput []
  = False
usesInput ((Getfield meth "Ljava/util/Scanner;"):_)
  = True
    where
      meth = thisClass++"/_scanner"
usesInput (_:rest)
  = usesInput rest

inputConstructor :: JProgram
inputConstructor
  = [Field "_scanner" "Ljava/util/Scanner;"] ++
    [Constructor
     ([ALoad_0]                                                             ++
     [New "java/util/Scanner"]                                              ++
     [Dup]                                                                  ++
     [Getstatic "java/lang/System.in" "Ljava/io/InputStream;"]              ++
     [Invokespecial "java/util/Scanner/<init>" "Ljava/io/InputStream;" "V"] ++
     [Putfield (thisClass++"/_scanner") "Ljava/util/Scanner;"])]

setupInputIfRequired :: JProgram -> JProgram
setupInputIfRequired program
  | usesInput program = inputConstructor ++ program
  | otherwise         = program
