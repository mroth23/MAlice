module MAlice.CodeGen.JavaBytecodeLocals where

import MAlice.CodeGen.JavaBytecodeInstr

setLocalVarNums :: JProgram -> JProgram
setLocalVarNums []
  = []
setLocalVarNums ((Func label params return numParams):rest)
  = (Func label params return numParams):(LocalsLimit num):
    (setLocalVarNums rest)
      where
	num = 1 + (max numParams (getNumOfLocalVars rest))
setLocalVarNums (instr:rest)
  = instr:(setLocalVarNums rest)

getNumOfLocalVars :: JProgram -> Int
getNumOfLocalVars ((Endmethod):rest)
  = 0
getNumOfLocalVars ((IStore num):rest)
  = max num (getNumOfLocalVars rest)
getNumOfLocalVars ((IStore_1):rest)
  = max 1 (getNumOfLocalVars rest)
getNumOfLocalVars ((IStore_2):rest)
  = max 2 (getNumOfLocalVars rest)
getNumOfLocalVars ((IStore_3):rest)
  = max 3 (getNumOfLocalVars rest)
getNumOfLocalVars ((AStore num):rest)
  = max num (getNumOfLocalVars rest)
getNumOfLocalVars ((AStore_1):rest)
  = max 1 (getNumOfLocalVars rest)
getNumOfLocalVars ((AStore_2):rest)
  = max 2 (getNumOfLocalVars rest)
getNumOfLocalVars ((AStore_3):rest)
  = max 3 (getNumOfLocalVars rest)
getNumOfLocalVars (instr:rest)
  = getNumOfLocalVars rest
