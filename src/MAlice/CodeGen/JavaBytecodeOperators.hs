module MAlice.CodeGen.JavaBytecodeOperators where

-- Module which return the instructions and 
-- updated LabelTable for a given operator
-- in a tuple.

import MAlice.CodeGen.JavaBytecodeInstr
import MAlice.CodeGen.JavaBytecodeUtil


-- Binary operators.
translateBinOp :: String -> LabelTable -> (JProgram, LabelTable)
translateBinOp "+" labelTable  = ([IAdd], labelTable)
translateBinOp "-" labelTable  = ([ISub], labelTable)
translateBinOp "*" labelTable  = ([IMul], labelTable)
translateBinOp "/" labelTable  = ([IDiv], labelTable)
translateBinOp "%" labelTable  = ([IRem], labelTable)
translateBinOp "|" labelTable  = ([IOr], labelTable)
translateBinOp "^" labelTable  = ([IXor], labelTable)
translateBinOp "&" labelTable  = ([IAnd], labelTable)
translateBinOp "||" labelTable = ([IOr], labelTable)
translateBinOp "&&" labelTable = ([IAnd], labelTable)
translateBinOp "==" labelTable
  = ([If_icmpeq label]   ++
    booleanCode,
    labelTable')
      where
        (booleanCode, label, labelTable') = setBooleanCode labelTable
translateBinOp "!=" labelTable
  = ([If_icmpne label] ++
    booleanCode,
    labelTable')
      where
        (booleanCode, label, labelTable') = setBooleanCode labelTable
translateBinOp ">" labelTable
  = ([If_icmpgt label] ++
    booleanCode,
    labelTable')
      where
        (booleanCode, label, labelTable') = setBooleanCode labelTable
translateBinOp "<" labelTable
  = ([If_icmplt label] ++
    booleanCode,
    labelTable')
      where
        (booleanCode, label, labelTable') = setBooleanCode labelTable
translateBinOp ">=" labelTable
  = ([If_icmpge label] ++
    booleanCode,
    labelTable')
      where
        (booleanCode, label, labelTable') = setBooleanCode labelTable
translateBinOp "<=" labelTable
  = ([If_icmple label] ++
     booleanCode,
     labelTable')
       where
         (booleanCode, label, labelTable') = setBooleanCode labelTable

-- Unary operators.
translateUnOp :: String -> JProgram
translateUnOp "-" = [INeg]
translateUnOp "+" = []
translateUnOp "~" = [INeg]
translateUnOp "!" = [(Ldc (ConsI 1)),(IXor)]
