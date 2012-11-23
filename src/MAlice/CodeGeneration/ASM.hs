module MAlice.CodeGeneration.ASM where

import Data.Word

data Register = RAX | RBX | RCX | RDX
data Instruction = Mov

type ImmVal = Word64
type Adress = Word64

-- etc