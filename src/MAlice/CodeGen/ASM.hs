module MAlice.CodeGen.ASM where

import MAlice.Language.Types
import MAlice.Language.AST
import Data.Word

data Register
  = RAX | RBX | RCX | RDX | R8 | R9 | R10 | R11 | R12 | R13 | R14
  deriving (Show, Eq)

data Instruction
  = Mov   Operand Operand
  | Push  Operand
  | Pop   Operand
  | Add   Operand Operand
  | Sub   Operand Operand
  | Mul   Operand Operand
  | Div   Operand Operand
  | IDiv  Operand Operand
  | Mod   Operand Operand -- Use DIV and find value in EDX
  | Neg   Operand
  | Not   Operand
  | Or    Operand Operand
  | And   Operand Operand
  | Xor   Operand Operand
  | Inc   Operand
  | Dec   Operand
  | Test  Operand Operand
  | Label String
  | Jmp   String
  | Jng   String
  | Jg    String
  | Jnge  String
  | Jge   String
  | Je    String
  | Cmp   Operand Operand
  | Call  String
  | Ret
  deriving (Show, Eq)

data Operand
  = Reg Register
  | Imm Integer
  deriving (Show, Eq)

type ImmVal = Word64
type Adress = Word64

type VariableStore = [VariableLoc]

data VariableLoc
  = Regis String Register | Addre String Int
  deriving (Show, Eq)

-- Returns a variable location given a string key and a variable store.
getVariableLoc :: String -> VariableStore -> VariableLoc
getVariableLoc key []
  = error "Key not found in variable store."
getVariableLoc key ((Regis str reg):rest)
  | key == str = Regis str reg
getVariableLoc key ((Addre str int):rest)
  | key == str = Addre str int
getVariableLoc key (_:rest)
  = getVariableLoc key rest


-- Inserts a variable location into the variable store.
insertVariableLoc :: VariableLoc -> VariableStore -> VariableStore
insertVariableLoc varLoc varStore
  = varStore ++ [varLoc]


-- Removes variable location store, I hope there is only one in there...
removeVariableLoc :: String -> VariableStore -> VariableStore
removeVariableLoc key []
  = []
removeVariableLoc key ((Regis str reg):xs)
  | key == str = xs
removeVariableLoc key ((Addre str int):xs)
  | key == str = xs
removeVariableLoc key (x:xs)
  = [x] ++ removeVariableLoc key xs
