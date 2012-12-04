module MAlice.IR.Types where

import MAlice.Language.AST
import MAlice.Language.Types

type IntermediateCode = [Instr]
-- We can only operate on (temporary) labels
type Operand = String



data Instr =
  BinOp BinaryOperator Operand Operand
  Label String |
  Jsr String |
  Ret |
  Mov Operand Operand