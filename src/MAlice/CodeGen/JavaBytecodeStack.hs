module MAlice.CodeGen.JavaBytecodeStack where

-- Module that sets up all the function stacks
-- in the program. This is because stack size
-- is required and a default might be wrong
-- and will near always be inefficient so 
-- we work out what the stack sizes
-- of functions should be.
-- This code walks the list of instructions
-- and keeps track of the size of the stack
-- returning the max value of the stack at 
-- any point.
-- Jumps and conditions are in this walking
-- so it follows all possible paths (and
-- potentially some impossible ones too).

import MAlice.CodeGen.JavaBytecodeInstr
import MAlice.CodeGen.JavaBytecodeUtil

setupMethodStacks :: JProgram -> JProgram
setupMethodStacks []
  = []
setupMethodStacks ((Func ident param ret n):rest)
  = [Func ident param ret n] ++
    [StackLimit num]         ++
    body                     ++
    setupMethodStacks rest'
      where
        (body, rest') = splitFunctionFromProgram rest
        num           = calculateStackSize body (0, 0)
setupMethodStacks (instr:rest)
  = instr:(setupMethodStacks rest)

calculateStackSize :: JProgram -> (Int, Int) -> Int
calculateStackSize [] (_, m)
  = m
calculateStackSize ((ALoad _):rest) (curr, m)
  = calculateStackSize rest (curr+1, max (curr+1) m)
calculateStackSize ((AStore _):rest) (curr, m)
  = calculateStackSize rest (curr-1, m)
calculateStackSize ((ILoad _):rest) (curr, m)
  = calculateStackSize rest (curr+1, max (curr+1) m)
calculateStackSize ((IStore _):rest) (curr, m)
  = calculateStackSize rest (curr-1, m)
calculateStackSize ((Putfield _ _):rest) (curr, m)
  = calculateStackSize rest (curr-2, m)
calculateStackSize ((Call _ paramString _):rest) (curr, m)
  = calculateStackSize rest (curr-params, m)
    where
      params = getParamsNumber paramString
calculateStackSize ((Ifnull label):rest) (curr, m)
  = max (calculateStackSize rest (curr-1, m)) (calculateStackSize rest' (curr-1, m))
    where
      rest' = jumpToLabel rest label
calculateStackSize ((Ldc _):rest) (curr, m)
  = calculateStackSize rest (curr+1, max (curr+1) m)
calculateStackSize ((Goto label):rest) (curr, m)
  = calculateStackSize rest' (curr, m)
    where
      rest' = jumpToLabel rest label
calculateStackSize ((If_icmpeq label):rest) (curr, m)
  = max (calculateStackSize rest (curr-1, m)) (calculateStackSize rest' (curr-1, m))
    where
      rest' = jumpToLabel rest label
calculateStackSize ((If_icmpne label):rest) (curr, m)
  = max (calculateStackSize rest (curr-1, m)) (calculateStackSize rest' (curr-1, m))
    where
      rest' = jumpToLabel rest label
calculateStackSize ((If_icmplt label):rest) (curr, m)
  = max (calculateStackSize rest (curr-1, m)) (calculateStackSize rest' (curr-1, m))
    where
      rest' = jumpToLabel rest label
calculateStackSize ((If_icmpgt label):rest) (curr, m)
  = max (calculateStackSize rest (curr-1, m)) (calculateStackSize rest' (curr-1, m))
    where
      rest' = jumpToLabel rest label
calculateStackSize ((If_icmple label):rest) (curr, m)
  = max (calculateStackSize rest (curr-1, m)) (calculateStackSize rest' (curr-1, m))
    where
      rest' = jumpToLabel rest label
calculateStackSize ((If_icmpge label):rest) (curr, m)
  = max (calculateStackSize rest (curr-1, m)) (calculateStackSize rest' (curr-1, m))
    where
      rest' = jumpToLabel rest label
calculateStackSize ((Ifeq label):rest) (curr, m)
  = max (calculateStackSize rest (curr-1, m)) (calculateStackSize rest' (curr-1, m))
    where
      rest' = jumpToLabel rest label
calculateStackSize ((Ifne label):rest) (curr, m)
  = max (calculateStackSize rest (curr-1, m)) (calculateStackSize rest' (curr-1, m))
    where
      rest' = jumpToLabel rest label
calculateStackSize ((Ifge label):rest) (curr, m)
  = max (calculateStackSize rest (curr-1, m)) (calculateStackSize rest' (curr-1, m))
    where
      rest' = jumpToLabel rest label
calculateStackSize ((Ifgt label):rest) (curr, m)
  = max (calculateStackSize rest (curr-1, m)) (calculateStackSize rest' (curr-1, m))
    where
      rest' = jumpToLabel rest label
calculateStackSize ((Ifle label):rest) (curr, m)
  = max (calculateStackSize rest (curr-1, m)) (calculateStackSize rest' (curr-1, m))
    where
      rest' = jumpToLabel rest label
calculateStackSize ((Iflt label):rest) (curr, m)
  = max (calculateStackSize rest (curr-1, m)) (calculateStackSize rest' (curr-1, m))
    where
      rest' = jumpToLabel rest label
calculateStackSize ((IAStore):rest) (curr, m)
  = calculateStackSize rest (curr-3, m)
calculateStackSize ((BIPush _):rest) (curr, m)
  = calculateStackSize rest (curr+1, max (curr+1) m)
calculateStackSize ((Checkcast _):rest) (curr, m)
  = calculateStackSize rest (curr-1, m)
calculateStackSize ((Getstatic _ _):rest) (curr, m)
  = calculateStackSize rest (curr+1, max (curr+1) m)
calculateStackSize ((Invokevirtual _ paramString _):rest) (curr, m)
  = calculateStackSize rest (curr-params, m)
    where
      params = getParamsNumber paramString
calculateStackSize ((Invokestatic _ paramString _):rest) (curr, m)
  = calculateStackSize rest (curr-params, m)
    where
      params = getParamsNumber paramString
calculateStackSize ((New _):rest) (curr, m)
  = calculateStackSize rest (curr+1, max (curr+1) m)
calculateStackSize (instr:rest) (curr, m)
  | elem instr l1Instrs = calculateStackSize rest (curr+1, max (curr+1) m)
  | elem instr r1Instrs = calculateStackSize rest (curr-1, m)
  | otherwise           = calculateStackSize rest (curr, m)


getParamsNumber :: String -> Int
getParamsNumber []
  = 0
getParamsNumber ('I':rest)
  = 1 + getParamsNumber rest
getParamsNumber ('C':rest)
  = 1 + getParamsNumber rest
getParamsNumber ('[':rest)
  = getParamsNumber rest
getParamsNumber ('L':rest)
  = 1 + getParamsNumber (tail (dropWhile (/=';') rest))

jumpToLabel :: JProgram -> String -> [JInstr]
jumpToLabel [] _
  = []
jumpToLabel ((LLabel label):rest)  str
  | label == str = rest
jumpToLabel (_:rest) str
  = jumpToLabel rest str

l1Instrs :: [JInstr]
l1Instrs =
  [ALoad_0]  ++
  [ALoad_1]  ++
  [ALoad_2]  ++
  [ALoad_3]  ++
  [ILoad_1]  ++
  [ILoad_2]  ++
  [ILoad_3]  ++
  [IConst_0] ++
  [IConst_1] ++
  [IConst_2] ++
  [IConst_3] ++
  [IConst_4] ++
  [IConst_5] ++
  [Dup]      ++
  [AConst_null] ++
  [NewAtomicReference]

r1Instrs :: [JInstr]
r1Instrs =
  [AStore_1] ++
  [AStore_2] ++
  [AStore_3] ++
  [IStore_1] ++
  [IStore_2] ++
  [IStore_3] ++
  [IAdd]     ++
  [IMul]     ++
  [ISub]     ++
  [IDiv]     ++
  [IRem]     ++
  [Pop]      ++
  [IAnd]     ++
  [IOr]      ++
  [IXor]     ++
  [IALoad]   ++
  [InvokeAtomicReference]
