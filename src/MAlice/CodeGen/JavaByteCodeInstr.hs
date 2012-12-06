data JInstr =
  Class Label              | -- Class definition with name.
  SuperClass               | -- Default super class is java/lang/Object.
  Call Label FParams RType | -- Call method with params and return type.
  ILoad Int                | -- Pushes value of local var. (Integer)
  IStore Int               | -- Pops value into local var. (Integer)
  Ldc Constant             | -- Push constant.
  Dup                      | -- Word on top of stack duplicated.
  Pop                      | -- Pops top word off stack.
  Swap                     | -- Swap two operand stack words.
  IAdd                     | -- Add two integers.
  IDiv                     | -- Divide two integers.
  IMul                     | -- Multiply two integers.
  ISub                     | -- Subtract two integers. (A-B, push A first)
  IRem                     | -- Logical int remainder.
  Goto Label               | -- Jump to label.
  Ifeq Label               | -- Jump to label if value on top of stack is 0.
  Ifge Label               | -- Jump to label if value on top of stack >= 0.
  Ifgt Label               | -- Jump to label if value on top of stack >  0.
  Ifle Label               | -- Jump to label if value on top of stack <= 0.
  Iflt Label               | -- Jump to label if value on top of stack <  0.
  Ifne Label               | -- Jump to label if value on top of stack != 0.
  IAnd                     | -- Bitwise AND of two integers.
  IOr                      | -- Bitwise OR of two integers.
  IShl                     | -- Int shift left.
  IShr                     | -- Arithemtic int shift right.
  IUshr                    | -- Logical int shift right.
  IXor                     | -- Xor two values on stack.
  IConst_0                 | -- Load int value 0 onto stack.
  IConst_1                 | -- Load int value 1 onto stack.
  I2c                      | -- Convert an int to a character.
  Newarray AType           | -- Create a new array with elements as int on stack.
  IALoad                   | -- Load an int from an array. Push array ref, then int index.
  IAStore                  | -- Load an int into an array. Push array ref, then int index, then int.
  IInc Index Constant      | -- Increment a local var by a given constant.
  IReturn                  | -- Return an integer from a function.
  Return                   | -- Return void from a method.


data Constant =
  ConsI Int   | 
  ConsS String

type Label = String
type AType = Int
type Index = Int
