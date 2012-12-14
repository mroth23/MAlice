module MAlice.CodeGen.JavaByteCodeUnitialisedReferences where

import MAlice.CodeGen.JavaByteCodeInstr
import MAlice.CodeGen.JavaByteCodeUtil

setupUnitialisedReferences :: JProgram -> LabelTable -> (JProgram, LabelTable)
setupUnitialisedReferences [] _
  = ([], [])
setupUnitialisedReferences ((Func label params return num):rest) labelTable
  = ([Func label params return num] ++
    body'                           ++
    rest'', labelTable'') 
      where
        (body, rest')                = splitFunctionFromProgram rest
	(body', _, _, labelTable')   = sur body (generateVarList num) [] labelTable
	(rest'', labelTable'')       = setupUnitialisedReferences rest' labelTable'
setupUnitialisedReferences (instr:rest) labelTable
  = ((instr):rest', labelTable')
    where
      (rest', labelTable') = setupUnitialisedReferences rest labelTable

--      Current    init     ninit     
sur :: JProgram -> [Int] -> [Int] -> LabelTable -> (JProgram, [Int], [Int], LabelTable)
sur [] _ _ _
  = ([], [], [], [])
sur ((IStore_1):rest) nums c l
  = ((IStore_1):rest', nums', c', l')
    where
      (rest', nums', c', l') = sur rest (1:nums) c l
sur ((IStore_2):rest) nums c l
  = ((IStore_2):rest', nums', c', l')
    where
      (rest', nums', c', l') = sur rest (2:nums) c l
sur ((IStore_3):rest) nums c l
  = ((IStore_3):rest', nums', c', l')
    where
      (rest', nums', c', l') = sur rest (3:nums) c l
sur ((AStore_1):rest) nums c l
  = ((AStore_1):rest', nums', c', l') 
    where
      (rest', nums', c', l') = sur rest (1:nums) c l
sur ((IStore num):rest) nums c l
  = ((IStore num):rest', nums', c', l')
    where
      (rest', nums', c', l') = sur rest (num:nums) c l
sur ((AStore_2):rest) nums c l
  = ((AStore_2):rest', nums', c', l')
    where
      (rest', nums', c', l') = sur rest (2:nums) c l
sur ((AStore_3):rest) nums c l
  = ((AStore_3):rest', nums', c', l')
    where
      (rest', nums', c', l') = sur rest (3:nums) c l
sur ((AStore num):rest) nums c l
  = ((AStore num):rest', nums', c', l')
    where
        (rest', nums', c', l') = sur rest (num:nums) c l
sur ((New "java/lang/Integer"):(Dup):(loadInstr):(invokeInstr1):(invokeInstr2):(storeInstr):rest) nums c l
  | elem loadNum nums = ((New "java/lang/Integer"):(Dup):(loadInstr):(invokeInstr1):(invokeInstr2):(storeInstr):rest', nums', c', l')
  | otherwise         = ((AConst_null):(invokeInstr2):(storeInstr):rest', nums', c', l')
    where
      (rest', nums', c', l') = sur rest nums modC l
      modC                   = if elem loadNum nums then c else (storeNum:c)
      loadNum                = loadVar loadInstr
      storeNum               = storeVar storeInstr
sur ((loadInstr):(invokeInstr1):(Checkcast str):(invokeInstr2):(storeInstr):rest) nums c l
  | not (elem loadNum c) = ((loadInstr):(invokeInstr1):(Checkcast str):(invokeInstr2):(storeInstr):rest', nums', c', l''')
  | otherwise            = ((loadInstr):(invokeInstr1):(Dup):(Ifnull isNullLabel):(Checkcast str):(invokeInstr2):(storeInstr):(Goto isNotNullLabel):(LLabel isNullLabel):(Pop):(LLabel isNotNullLabel):rest', nums', c', l''')
    where
      loadNum = loadVar loadInstr
      (isNullLabel, l') = generateNewLabel l
      (isNotNullLabel, l'') = generateNewLabel l'
      (rest', nums', c', l''') = sur rest nums c l''
sur (instr:rest) nums c l
  = (instr:rest', nums', c', l')
    where
      (rest', nums', c', l') = sur rest nums c l

loadVar :: JInstr -> Int
loadVar ILoad_1     = 1
loadVar ILoad_2     = 2
loadVar ILoad_3     = 3
loadVar (ILoad num) = num
loadVar ALoad_1     = 1
loadVar ALoad_2     = 2
loadVar ALoad_3     = 3
loadVar (ALoad num) = num

storeVar :: JInstr -> Int
storeVar AStore_1     = 1
storeVar AStore_2     = 2
storeVar AStore_3     = 3
storeVar (AStore num) = num
