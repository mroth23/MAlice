module MAlice.CodeGen.JavaBytecodeMissingReturns where

-- Module that will fix the missing returns on functions
-- that use returns in branch. JVM being statically type
-- and the bytecode verifier mean we have to do this.

import MAlice.CodeGen.JavaBytecodeInstr
import MAlice.CodeGen.JavaBytecodeUtil

-- JVM won't let you get to end of function with correct return.
setupMissingReturns :: JProgram -> JProgram
setupMissingReturns []
  = []
setupMissingReturns ((Func label params return num):rest)
  = [Func label params return num]   ++
    setupMissingReturn (body) return ++
    setupMissingReturns rest'
      where
        (body, rest') = splitFunctionFromProgram rest
setupMissingReturns (instr:rest)
  = (instr):(setupMissingReturns rest)

setupMissingReturn :: JProgram -> String -> JProgram
setupMissingReturn [] _
  = []
-- Checks for correct returns and adds them in if required.
setupMissingReturn (instr:[Endmethod]) str
  | str == "I" && instr /= (IReturn)
      = [instr]                     ++
        [ALoad_0]                   ++
        [Invokevirtual call "" "V"] ++
	[IConst_m1]                 ++
	[IReturn]                   ++
	[Endmethod]
  | str == "C" && instr /= (IReturn)
      = [instr]                     ++
        [ALoad_0]                   ++
        [Invokevirtual call "" "V"] ++
	[IConst_m1]                 ++
	[IReturn]                   ++
	[Endmethod]
  | str == "V" && instr /= (Return)
      = [instr]  ++
        [Return] ++
	[Endmethod]
  | str /= "V" && str /= "I" && str /= "C" && instr /= (AReturn)
      = [instr]                     ++
        [ALoad_0]                   ++
	[Invokevirtual call "" "V"] ++
	[AConst_null]               ++
	[AReturn]                   ++
	[Endmethod]
  | otherwise = instr:[Endmethod]
        where
	  call = (getClassName++"/"++"_throwConditionError")
setupMissingReturn [Endmethod] _
  = [Endmethod]
setupMissingReturn (instr:rest) str
  = (instr):(setupMissingReturn rest str)
