module MAlice.CodeGen.JavaByteCodeJunkLabels where

import MAlice.CodeGen.JavaByteCodeInstr
import MAlice.CodeGen.JavaByteCodeUtil

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
  | elem label labelTable = removeJunkLabels rest labelTable
  | otherwise             = (LLabel label):(removeJunkLabels rest labelTable)
removeJunkLabels ((Goto label):rest) labelTable
  | elem label labelTable = removeJunkLabels rest labelTable
  | otherwise             = (Goto label):(removeJunkLabels rest labelTable)
removeJunkLabels (somethingElse:rest) labelTable
  = (somethingElse):(removeJunkLabels rest labelTable)
