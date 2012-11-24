module MAlice.CodeGeneration.ASM where

import MAlice.Language.Types
import MAlice.Language.AST
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
  | Colon String
  deriving (Show, Eq)

type ImmVal = Word64
type Adress = Word64

type VariableStore = [VariableLoc]

data VariableLoc
  = Regis String Register | Addre String Int

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
removeVariableLoc key (_:xs)
  = removeVariableLoc key xs
