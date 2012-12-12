module MAlice.CodeGen.JavaByteCodeOptimiser where

import MAlice.CodeGen.JavaByteCodeInstr

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

optimise (instr:rest)
  = instr:(optimise rest)
