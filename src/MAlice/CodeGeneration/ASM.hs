module MAlice.CodeGeneration.ASM where

import Data.Word

data Register 
  = RAX | RBX | RCX | RDX | R8 | R9 | R10 | R11 | R12 | R13 | R14 
  deriving (Show, Eq)

data Instruction 
  = Mov Register Register
  | Push Register
  | Pop Register
  | Add Register Register
  | Sub Register Register
  deriving (Show, Eq)

type ImmVal = Word64
type Adress = Word64

-- etc
