module MAlice.CodeGen.JavaByteCodeInstr where 

type JProgram = [JInstr]

data JInstr =
  Class Label              | -- Class definition with name.
  SuperClass               | -- Default super class is java/lang/Object.
  MainMethod               | -- Main method when program executed.
  Constructor JProgram     | -- Constructor for this programs object.
  Field Label String       | -- Creates a new variable with label and type.
  Getfield Label String    | -- Gets field value from objectref.
  Putfield Label String    | -- Puts a objectref in the field.
  Call Label String String | -- Call method with params and return type.
  Func Label String String | -- A function.
  ALoad Int                | -- Load reference onto stack from local var int..
  ALoad_0                  | -- Load reference onto stack from local var 0.
  ALoad_1                  | -- Load object in local variable 1 onto stack.
  AStore Int               | -- Store object reference from stack in local var.
  ILoad_0                  |
  ILoad_1                  |
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
  IConst_m1                | -- Load int value -1 onto stack.
  I2c                      | -- Convert an int to a character.
  New String               | -- Create object of type string and place ref on stack.
  Newarray String          | -- Create a new array with elements as int on stack.
  IALoad                   | -- Load an int from an array. Push array ref, then int index.
  IAStore                  | -- Load an int into an array. Push array ref, then int index, then int.
  IInc Index Constant      | -- Increment a local var by a given constant.
  BIPush Int               | -- Push char onto stack
  Getstatic String String  | -- Get a static field value of a class.
  --            Ident  Param  Return
  Invokevirtual String String String  | -- Invokes/calls a method.
  Invokespecial String String String  | -- Special for init.
  Endmethod                | -- Signals the end of a method.
  IReturn                  | -- Return an integer from a function.
  Return                     -- Return void from a method.

instance Show JInstr where
  show (Class label)
    = ".class public " ++ label ++ "\n"
  show (SuperClass)
    = ".super java/lang/Object\n"
  show (MainMethod)
    = ".method public static main([Ljava/lang/String;)V\n" ++
      ".limit stack 100\n " ++
      ".limit locals 100\n " ++
      "new " ++ "Myclass" ++ "\n" ++
      "dup\n" ++
      "invokespecial " ++ "Myclass" ++ "/<init>()V \n" ++
      "astore_0\n" ++
      "return\n"   ++
      ".end method\n"
  show (Field label t)
    = ".field public " ++ label ++ " " ++ t ++ "\n"
  show (Getfield label return)
    = "getfield " ++ label ++ " " ++ return ++ "\n"
  show (Putfield label return)
    = "putfield " ++ label ++ " " ++ return ++ "\n"
  show (Call label params return)
    = "invokevirtual " ++
       label ++ "(" ++ params ++ ")" ++
       return ++ "\n"
  show (Func label params return)
    = ".method public " ++ 
       label ++ "(" ++ params ++ ")" ++ return ++ "\n" ++
       ".limit stack 100\n.limit locals 100\n"
  show (ALoad num)         = "aload " ++ show num ++ "\n"
  show (ALoad_0)           = "aload_0" ++ "\n"
  show (ALoad_1)           =  "aload_1" ++ "\n"
  show (AStore num)        = "astore " ++ show num ++ "\n"
  show (ILoad_0)           = "iload_0" ++ "\n"
  show (ILoad_1)           = "iload_1" ++ "\n"
  show (ILoad num)         =  "iload " ++ show num ++ "\n"
  show (IStore num)        = "istore " ++ show num ++ "\n"
  show (Ldc const)         = "ldc " ++ show const ++ "\n"
  show (Dup)               = "dup" ++ "\n"
  show (Pop)               = "pop" ++ "\n"
  show (Swap)              = "swap" ++ "\n"
  show (IAdd)              = "iadd" ++ "\n"
  show (IMul)              = "imul" ++ "\n"
  show (ISub)              = "isub" ++ "\n"
  show (IDiv)              = "idiv" ++ "\n"
  show (IRem)              = "irem" ++ "\n"
  show (Goto label)        = "goto " ++ label ++ "\n"
  show (Ifeq label)        = "ifeq " ++ label ++ "\n"
  show (Ifge label)        = "ifge " ++ label ++ "\n"
  show (Ifgt label)        = "ifgt " ++ label ++ "\n"
  show (Ifle label)        = "ifle " ++ label ++ "\n"
  show (Iflt label)        = "iflt " ++ label ++ "\n"
  show (Ifne label)        = "ifne " ++ label ++ "\n"
  show (IAnd)              = "iand" ++ "\n"
  show (IOr)               = "ior" ++ "\n"
  show (IShl)              = "ishl" ++ "\n"
  show (IShr)              = "ishr" ++ "\n"
  show (IUshr)             = "iushr" ++ "\n"
  show (IXor)              = "ixor" ++ "\n"
  show (IConst_0)          = "iconst_0" ++ "\n"
  show (IConst_1)          = "iconst_1" ++ "\n"
  show (IConst_m1)         = "iconst_m1" ++ "\n"
  show (I2c)               = "i2c" ++ "\n"
  show (New str)           = "new " ++ str ++ "\n"
  show (Newarray str)      = "newarray " ++ str ++ "\n"
  show (IALoad)            = "iaload" ++ "\n"
  show (IAStore)           = "iastore" ++ "\n"
  show (IInc index const)  = "iinc " ++ show index ++ " " ++ show const ++ "\n"
  show (BIPush i)          = "bipush " ++ show i ++ "\n"
  show (Endmethod)         = ".end method \n"
  show (IReturn)           = "iret" ++ "\n" 
  show (Return)            = "return" ++ "\n"
  show (Getstatic lib obj)
    = "getstatic " ++ lib ++ " " ++ obj ++ "\n"
  show (Invokevirtual label param ret)
    = "invokevirtual " ++
       label ++ "(" ++ param ++ ")" ++
       ret ++ "\n"
  show (Invokespecial label param ret)
    = "invokespecial " ++ 
       label ++ "(" ++ param ++ ")" ++
       ret ++ "\n"

data Constant =
  ConsI Int   | 
  ConsS String

instance Show Constant where
  show (ConsI i)
    = show i
  show (ConsS str)
    = show str

type FParams = [FParam]
data FParam =
  FInt     |
  FString  |
  FArray

{- data FType =
  FInt        |
  FString     |
  FArray FInt |
  FArray FType -}

type Label = String
type AType = Int
type Index = Int
