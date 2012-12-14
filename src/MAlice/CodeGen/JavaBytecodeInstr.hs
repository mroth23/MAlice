module MAlice.CodeGen.JavaBytecodeInstr where 

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
  Func Label String String Int | -- A function.
  ALoad Int                | -- Load reference onto stack from local var int..
  ALoad_0                  | -- Load reference onto stack from local var 0.
  ALoad_1                  | -- Load object in local variable 1 onto stack.
  ALoad_2                  |
  ALoad_3                  |
  AStore Int               | -- Store object reference from stack in local var.
  AStore_0                 |
  AStore_1                 |
  AStore_2                 |
  AStore_3                 |
  AAStore                  | -- Store reference to object on stack in array.
  AConst_null              |
  Ifnull String            |
  ILoad_0                  |
  ILoad_1                  |
  ILoad_2                  |
  ILoad_3                  |
  ILoad Int                | -- Pushes value of local var. (Integer)
  IStore Int               | -- Pops value into local var. (Integer)
  IStore_1                 |
  IStore_2                 |
  IStore_3                 |
  Ldc Constant             | -- Push constant.
  Dup                      | -- Word on top of stack duplicated.
  Pop                      | -- Pops top word off stack.
  Swap                     | -- Swap two operand stack words.
  IAdd                     | -- Add two integers.
  IDiv                     | -- Divide two integers.
  IMul                     | -- Multiply two integers.
  ISub                     | -- Subtract two integers. (A-B, push A first)
  IRem                     | -- Logical int remainder.
  INeg                     | -- Negate int.
  Goto Label               | -- Jump to label.
  LLabel Label             | -- Label definition.
  If_icmpeq Label          | -- Jump to label if equal.
  If_icmpne Label          | -- Jump to label if not equal.
  If_icmplt Label          | -- Jump to label if less than.
  If_icmpgt Label          | -- Jump to label if greater than.
  If_icmple Label          | -- Jump to label if less than or equal.
  If_icmpge Label          | -- Jump to label if greater than or equal.
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
  IConst_2                 |
  IConst_3                 |
  IConst_4                 |
  IConst_5                 |
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
  Invokestatic String String String   | -- Static calls.
  Endmethod                | -- Signals the end of a method.
  IReturn                  | -- Return an integer from a function.
  AReturn                  | -- Return a reference from a function.
  Return                   | -- Return void from a method.
  NewAtomicReference       |
  InvokeAtomicReference    |
  StackLimit Int           |
  LocalsLimit Int          |
  Checkcast String         |
  ThrowConditionError
    deriving Eq

instance Show JInstr where
  show (Class label)
    = ".class public " ++ label ++ "\n"
  show (SuperClass)
    = ".super java/lang/Object\n"
  show (MainMethod)
    = ".method public static main([Ljava/lang/String;)V\n" ++
      ".limit stack 2\n " ++
      ".limit locals 1\n " ++
      "new " ++ "Myclass" ++ "\n" ++
      "dup\n" ++
      "invokespecial " ++ "Myclass" ++ "/<init>()V\n" ++
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
  show (Func label params return _)
    = ".method public " ++ 
       label ++ "(" ++ params ++ ")" ++ return ++ "\n"
  show (ALoad num)         = "aload " ++ show num ++ "\n"
  show (ALoad_0)           = "aload_0\n"
  show (ALoad_1)           = "aload_1\n"
  show (ALoad_2)           = "aload_2\n"
  show (ALoad_3)           = "aload_3\n"
  show (AStore num)        = "astore " ++ show num ++ "\n"
  show (AStore_0)          = "astore_0\n"
  show (AStore_1)          = "astore_1\n"
  show (AStore_2)          = "astore_2\n"
  show (AStore_3)          = "astore_3\n"
  show (AConst_null)       = "aconst_null\n"
  show (Ifnull str)        = "ifnull " ++ str ++ "\n"
  show (ILoad_0)           = "iload_0\n"
  show (ILoad_1)           = "iload_1\n"
  show (ILoad_2)           = "iload_2\n"
  show (ILoad_3)           = "iload_3\n"
  show (ILoad num)         = "iload " ++ show num ++ "\n"
  show (IStore num)        = "istore " ++ show num ++ "\n"
  show (IStore_1)          = "istore_1\n"
  show (IStore_2)          = "istore_2\n"
  show (IStore_3)          = "istore_3\n"
  show (Ldc const)         = "ldc " ++ show const ++ "\n"
  show (Dup)               = "dup\n"
  show (Pop)               = "pop\n"
  show (Swap)              = "swap\n"
  show (IAdd)              = "iadd\n"
  show (IMul)              = "imul\n"
  show (ISub)              = "isub\n"
  show (IDiv)              = "idiv\n"
  show (IRem)              = "irem\n"
  show (INeg)              = "ineg\n"
  show (LLabel label)      = label ++ ":\n"
  show (Goto label)        = "goto " ++ label ++ "\n"
  show (If_icmpeq label)   = "if_icmpeq " ++ label ++ "\n"
  show (If_icmpne label)   = "if_icmpne " ++ label ++ "\n"
  show (If_icmplt label)   = "if_icmplt " ++ label ++ "\n"
  show (If_icmpgt label)   = "if_icmpgt " ++ label ++ "\n"
  show (If_icmple label)   = "if_icmple " ++ label ++ "\n"
  show (If_icmpge label)   = "if_icmpge " ++ label ++ "\n"
  show (Ifeq label)        = "ifeq " ++ label ++ "\n"
  show (Ifge label)        = "ifge " ++ label ++ "\n"
  show (Ifgt label)        = "ifgt " ++ label ++ "\n"
  show (Ifle label)        = "ifle " ++ label ++ "\n"
  show (Iflt label)        = "iflt " ++ label ++ "\n"
  show (Ifne label)        = "ifne " ++ label ++ "\n"
  show (IAnd)              = "iand\n"
  show (IOr)               = "ior\n"
  show (IShl)              = "ishl\n"
  show (IShr)              = "ishr\n"
  show (IUshr)             = "iushr\n"
  show (IXor)              = "ixor\n"
  show (IConst_0)          = "iconst_0\n"
  show (IConst_1)          = "iconst_1\n"
  show (IConst_2)          = "iconst_2\n"
  show (IConst_3)          = "iconst_3\n"
  show (IConst_4)          = "iconst_4\n"
  show (IConst_5)          = "iconst_5\n"
  show (IConst_m1)         = "iconst_m1\n"
  show (I2c)               = "i2c" ++ "\n"
  show (New str)           = "new " ++ str ++ "\n"
  show (Newarray str)      = "newarray " ++ str ++ "\n"
  show (IALoad)            = "iaload\n"
  show (IAStore)           = "iastore\n"
  show (IInc index const)  = "iinc " ++ show index ++ " " ++ show const ++ "\n"
  show (BIPush i)          = "bipush " ++ show i ++ "\n"
  show (Endmethod)         = ".end method\n"
  show (IReturn)           = "ireturn\n"
  show (AReturn)           = "areturn\n" 
  show (Return)            = "return\n"
  show (LocalsLimit num)   = ".limit locals " ++ show num ++ "\n"
  show (StackLimit num)    = ".limit stack " ++ show num ++ "\n"
  show (Checkcast str)     = "checkcast " ++ str ++ "\n"
  show (NewAtomicReference)= "new java/util/concurrent/atomic/AtomicReference\n"
  show (InvokeAtomicReference) = "invokespecial java/util/concurrent/atomic/AtomicReference/<init>(Ljava/lang/Object;)V\n"
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
  show (Invokestatic label param ret)
    = "invokestatic " ++
      label ++ "(" ++ param ++ ")" ++
      ret ++ "\n"
  show (ThrowConditionError)
    = show (Func "_throwConditionError" "" "V" 0)                               ++
      ".limit locals 1\n"                                                       ++
      show (Getstatic "java/lang/System/out" "Ljava/io/PrintStream;")           ++
      show (Ldc (ConsS "No return matched in function."))                       ++
      show (Invokevirtual "java/io/PrintStream/print" "Ljava/lang/String;" "V") ++
      show (IConst_1)                                                           ++
      show (Invokestatic "java/lang/System/exit" "I" "V")                       ++
      show (Return)                                                             ++
      show (Endmethod)

data Constant =
  ConsI Int   | 
  ConsS String
  deriving Eq

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
