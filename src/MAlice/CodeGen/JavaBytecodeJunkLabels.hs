module MAlice.CodeGen.JavaBytecodeJunkLabels where

-- Module to allow you to retrieve labels not needed.
-- Also allows to remove ones from a list.

import MAlice.CodeGen.JavaBytecodeInstr
import MAlice.CodeGen.JavaBytecodeUtil


-- Get all the labels in the program that don't
-- make any difference to the program.
getJunkLabels :: JProgram -> LabelTable
getJunkLabels []
  = []
getJunkLabels ((LLabel label):(Endmethod):rest)
  = (label):(getJunkLabels rest)
getJunkLabels (somethingElse:rest)
  = getJunkLabels rest

removeJunkLabels :: JProgram -> LabelTable -> JProgram
removeJunkLabels [] labelTable
  = []
removeJunkLabels ((LLabel label):rest) labelTable
  | elem label labelTable 
    = removeJunkLabels rest labelTable
  | otherwise             
    = (LLabel label):(removeJunkLabels rest labelTable)
removeJunkLabels ((Goto label):rest) labelTable
  | elem label labelTable 
    = removeJunkLabels rest labelTable
  | otherwise             
    = (Goto label):(removeJunkLabels rest labelTable)
removeJunkLabels (somethingElse:rest) labelTable
  = (somethingElse):(removeJunkLabels rest labelTable)
