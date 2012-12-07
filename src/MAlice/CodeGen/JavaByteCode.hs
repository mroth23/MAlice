import MAlice.CodeGen.JavaByteCodeInstr
import MAlice.IR.Types

translate :: IRCode -> JProgram
translate intrs =
  [Class "my_class"] ++
  [SuperClass]   ++
  translateIntrs instrs

translateInstrs :: IRCode -> JProgram
translateInstrs []
  = []
translateInstrs (IAlloc label t):rest
  = [Field label t] ++ 
    translateIntrs rest
translateInstrs (IAllocArr label t num):rest
  = [ALoad_0]          ++
    [Field label "["t] ++
    [Push num]         ++ 
    [Newarray t]       ++
    [Putfield label]   

