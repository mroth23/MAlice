module MAlice.CodeGen.JavaByteCodeUtil where

import MAlice.CodeGen.JavaByteCodeInstr
import MAlice.Language.Types

thisClass :: String
thisClass = "Myclass"

-- Given code will extract the function code and return you rest too.
splitFunctionFromProgram :: JProgram -> (JProgram, JProgram)
splitFunctionFromProgram []
  = ([], [])
splitFunctionFromProgram ((Endmethod):rest)
  = ([Endmethod], rest)
splitFunctionFromProgram (instr:rest)
  = (instr:body, rest')
      where
        (body, rest') = splitFunctionFromProgram rest

generateVarList :: Int -> [Int]
generateVarList 0
  = [0]
generateVarList n
  = n:(generateVarList (n-1))

type LabelTable = [String]
-- Gives a brand new label to use given a list of already used labels.
generateNewLabel :: LabelTable -> (String, LabelTable)
generateNewLabel labelTable
  = generateNewLabel' labelTable 0
generateNewLabel' :: LabelTable -> Int -> (String, LabelTable)
generateNewLabel' labelTable int
  | elem label labelTable           = generateNewLabel' labelTable (int+1)
  | otherwise                       = (label, label:labelTable)
    where
      label = "_label" ++ show int

type VarTable = [VarTableEntry]

data VarTableEntry
  --       Ident  Type           Ident  Loc Type
  = Global String String | Local String Int String
    deriving (Show, Eq)
lookupVarTableEntry :: String -> VarTable -> VarTableEntry
lookupVarTableEntry _ []
  = (Local "x" 9999 "I")
lookupVarTableEntry str ((Global ident t):rest)
  | str == ident = (Global ident t)
lookupVarTableEntry str ((Local ident int t):rest)
  | str == ident = (Local ident int t)
lookupVarTableEntry str (entry:rest)
  = lookupVarTableEntry str rest

type MethTable = [MethTableEntry]

data MethTableEntry
    --    Ident  Param  Return
  = Entry String String String

lookupMethTableEntry :: String -> MethTable -> MethTableEntry
lookupMethTableEntry str ((Entry ident param ret):rest)
  | str == ident = (Entry ident param ret)
  | otherwise    = lookupMethTableEntry str rest

getNewLocalVar :: VarTable -> Int -> Int
getNewLocalVar table num
  | tryLocalVar table num = num
  | otherwise             = getNewLocalVar table (num+1)
tryLocalVar :: VarTable -> Int -> Bool
tryLocalVar [] num
  = True
tryLocalVar ((Local ident int t):rest) num
  | num == int = False
tryLocalVar (_:rest) num
  = tryLocalVar rest num

-- Output the instructions in a more readable format.
showJavaProgram :: JProgram -> IO ()
showJavaProgram program
  = putStr (getJavaProgramString program)
getJavaProgramString :: JProgram -> String
getJavaProgramString []
  = ""
getJavaProgramString ((Constructor program):rest)
  = ".method public <init>()V\n"  ++
    " .limit stack 100\n"                             ++
    " .limit locals 100\n"                            ++
    "aload_0\n"                                       ++
    "dup\n"                                           ++
    "invokespecial java/lang/Object/<init>()V\n"      ++
    constructorCode                                    ++
    "invokevirtual " ++ thisClass ++ "/hatta()V\n"    ++
    "return\n"                                        ++
    ".end method\n"                                   ++
    rest'
      where
        constructorCode = getJavaProgramString program
        rest'           = getJavaProgramString rest
getJavaProgramString (instr:rest)
  = (show instr) ++ (getJavaProgramString rest)

-- Move fields to the top of the program, required for valid bytecode.
moveFieldsToTop :: JProgram -> JProgram
moveFieldsToTop program
  = fields ++ rest
    where
      (fields, rest) = moveFieldsToTop' program
moveFieldsToTop' :: JProgram -> (JProgram, JProgram)
moveFieldsToTop' []
  = ([], [])
moveFieldsToTop' ((Field label t):rest)
  = (((Field label t):fields), rest')
    where
      (fields, rest') = moveFieldsToTop' rest
moveFieldsToTop' (instr:rest)
  = (fields, instr:rest')
    where
      (fields, rest') = moveFieldsToTop' rest

-- Merge constructor code into one overall constructor.
mergeConstructors :: JProgram -> JProgram
mergeConstructors program
  = [Constructor (constructors)] ++ rest
    where
      (constructors, rest) = mergeConstructors' program
mergeConstructors' :: JProgram -> (JProgram, JProgram)
mergeConstructors' []
  = ([], [])
mergeConstructors' ((Constructor program):rest)
  = ((program++cons), rest')
    where
      (cons, rest') = mergeConstructors' rest
mergeConstructors' (instr:rest)
  = (cons, instr:rest')
    where
      (cons, rest') = mergeConstructors' rest

-- Because we can only input strings, we take the 0th char from that string.
inputCharHandling :: Type -> JProgram
inputCharHandling Letter
  = [Ldc (ConsI 0)] ++
    [Invokevirtual "java/lang/String/sharAt" "I" "C"]
inputCharHandling _
  = []
-- Internally chars are actually stored as ints, so when printing we must conver them.
printCharHandling :: Type -> JProgram
printCharHandling Letter
  = [I2c]
printCharHandling _
  = []

-- Some simple code to deal with booleans.
setBooleanCode :: LabelTable -> (JProgram, Label, LabelTable)
setBooleanCode labelTable
  = ([Ldc (ConsI 0)] ++
    [Goto label']   ++
    [LLabel label]  ++
    [Ldc (ConsI 1)] ++
    [LLabel label'],
    label, labelTable'')
      where
        (label, labelTable')   = generateNewLabel labelTable
        (label', labelTable'') = generateNewLabel labelTable'

-- Swap out a constructor instruction for lower level code.
convertConstructor :: JProgram -> JProgram
convertConstructor []
  = []
convertConstructor ((Constructor program):rest)
  = (convertConstructor' program) ++ rest
convertConstructor (instr:rest)
  = instr:(convertConstructor rest)

convertConstructor' :: JProgram -> JProgram
convertConstructor' program
  = [Func  "<init>" "" "V" 0]                        ++
    [ALoad_0]                                        ++
    [Dup]                                            ++
    [Invokespecial "java/lang/Object/<init>" "" "V"] ++
    program                                          ++
    [Invokevirtual hatta "" "V"]                     ++
    [Return]                                         ++
    [Endmethod]
      where
        hatta = thisClass++"/"++"hatta"
