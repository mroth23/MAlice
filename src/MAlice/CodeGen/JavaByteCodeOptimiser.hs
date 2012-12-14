module MAlice.CodeGen.JavaByteCodeOptimiser where

import MAlice.CodeGen.JavaByteCodeInstr

opt :: JProgram -> JProgram
opt program
  = program''
    where 
      program'  = optimise program
      program'' = setLocalVarNums program'

-- Optimise bytecode instructions.
optimise :: JProgram -> JProgram
optimise []
  = []
-- Optimise iload instructions.
optimise ((ILoad 1):rest)
  = (ILoad_1):(optimise rest)
optimise ((ILoad 2):rest)
  = (ILoad_2):(optimise rest)
optimise ((ILoad 3):rest)
  = (ILoad_3):(optimise rest)
-- Optimise istore instrcutions.
optimise ((IStore 1):rest)
  = (IStore_1):(optimise rest)
optimise ((IStore 2):rest)
  = (IStore_2):(optimise rest)
optimise ((IStore 3):rest)
  = (IStore_3):(optimise rest)
-- Optimise aload instructions.
optimise ((ALoad 1):rest)
  = (ALoad_1):(optimise rest)
optimise ((ALoad 2):rest)
  = (ALoad_2):(optimise rest)
optimise ((ALoad 3):rest)
  = (ALoad_3):(optimise rest)
-- Optimise astore instructions.
optimise ((AStore 0):rest)
  = (AStore_0):(optimise rest)
optimise ((AStore 1):rest)
  = (AStore_1):(optimise rest)
optimise ((AStore 2):rest)
  = (AStore_2):(optimise rest)
optimise ((AStore 3):rest)
  = (AStore_3):(optimise rest)
-- Optimise ldc instructions.
optimise ((Ldc (ConsI (-1))):rest)
  = (IConst_m1):(optimise rest)
optimise ((Ldc (ConsI 0)):rest)
  = (IConst_0):(optimise rest)
optimise ((Ldc (ConsI 1)):rest)
  = (IConst_1):(optimise rest)
optimise ((Ldc (ConsI 2)):rest)
  = (IConst_2):(optimise rest)
optimise ((Ldc (ConsI 3)):rest)
  = (IConst_3):(optimise rest)
optimise ((Ldc (ConsI 4)):rest)
  = (IConst_4):(optimise rest)
optimise ((Ldc (ConsI 5)):rest)
  = (IConst_5):(optimise rest)
-- Optimise duplicate constant loads.
optimise ((Ldc const1):(Ldc const2):rest)
  | const1 == const2 = (Ldc const1):(Dup):(optimise rest)
-- Optimise duplicate iload loads.
optimise ((ILoad num1):(ILoad num2):rest)
  | num1 == num2 = (ILoad num1):(Dup):(optimise rest)
optimise ((ILoad_1):(ILoad_1):rest)
  = (ILoad_1):(Dup):(optimise rest)
optimise ((ILoad_2):(ILoad_2):rest)
  = (ILoad_2):(Dup):(optimise rest)
optimise ((ILoad_3):(ILoad_3):rest)
  = (ILoad_3):(Dup):(optimise rest)
-- Optimise duplicate aload loads.
optimise ((ALoad num1):(ALoad num2):rest)
  | num1 == num2 = (ALoad num1):(Dup):(optimise rest)
optimise ((ALoad_0):(ALoad_0):rest)
  = (ALoad_0):(Dup):(optimise rest)
optimise ((ALoad_1):(ALoad_1):rest)
  = (ALoad_1):(Dup):(optimise rest)
optimise ((ALoad_2):(ALoad_2):rest)
  = (ALoad_2):(Dup):(optimise rest)
optimise ((ALoad_3):(ALoad_3):rest)
  = (ALoad_3):(Dup):(optimise rest)
-- Optimise store then load.
optimise ((IStore num1):(ILoad num2):rest)
  = (Dup):(IStore num1):(optimise rest)
optimise ((IStore_1):(ILoad_1):rest)
  = (Dup):(IStore_1):(optimise rest)
optimise ((IStore_2):(ILoad_2):rest)
  = (Dup):(IStore_2):(optimise rest)
optimise ((IStore_3):(ILoad_3):rest)
  = (Dup):(IStore_3):(optimise rest)
optimise ((AStore_1):(ALoad_1):rest)
  = (Dup):(AStore_1):(optimise rest)
optimise ((AStore_2):(ALoad_2):rest)
  = (Dup):(AStore_2):(optimise rest)
optimise ((AStore_3):(ALoad_3):rest)
  = (Dup):(AStore_3):(optimise rest)
optimise (instr:rest)
  = instr:(optimise rest)

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
