module MAlice.CodeGen.JavaByteCodeOperators where

import MAlice.CodeGen.JavaByteCodeInstr
import MAlice.CodeGen.JavaByteCodeUtil

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

translateUnOp :: String -> JProgram
translateUnOp "-" = [INeg]
translateUnOp "+" = []
translateUnOp "~" = [INeg]
translateUnOp "!" = [(Ldc (ConsI 1)),(IXor)]
