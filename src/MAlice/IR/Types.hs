module MAlice.IR.Types where

import qualified MAlice.Language.Types as MAlice
import qualified MAlice.Language.AST as AST
import MAlice.Language.Types

import Data.Maybe
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State

newtype Program = Program { code :: Code }

showProgram :: Program -> IO ()
showProgram irp = mapM_ print $ code irp

type Code = [Instr]

type CodeGen a = StateT CState Identity a
type SymbolTable  = M.Map String String

data CState = CState { lblCount :: Int
                     , symTables :: [SymbolTable] }

initState = CState { lblCount = 0
                   , symTables = [M.empty] }

getSymbolTables :: CodeGen [SymbolTable]
getSymbolTables =
  symTables `liftM` get

insertSymbol :: String -> String -> CodeGen ()
insertSymbol k e = do
  (t:ts) <- getSymbolTables
  let newt = M.insert k e t
  updateState $ \st -> st { symTables = newt : ts }

newSymbolTable :: CodeGen ()
newSymbolTable = do
  ts <- getSymbolTables
  updateState $ \st -> st { symTables = M.empty : ts }

removeSymbolTable :: CodeGen ()
removeSymbolTable = do
  (t : ts) <- getSymbolTables
  updateState $ \st -> st { symTables = ts }

updateState :: (CState -> CState) -> CodeGen ()
updateState f = do
  st <- get
  put $ f st

getDefinition :: String -> CodeGen String
getDefinition v = (fromJust . lookupInTables v) `liftM` getSymbolTables

lookupInTables :: String -> [SymbolTable] -> Maybe String
lookupInTables _ [] = Nothing
lookupInTables s (t : ts) =
  case M.lookup s t of
    Nothing -> lookupInTables s ts
    Just e -> Just e

globalPrefix = "__global_"
paramPrefix = "__param_"
localPrefix = "__var_"
methodPrefix = "__m_"
labelPrefix = "__label_"

uniqueNumber = do
  rval <- lblCount `liftM` get
  updateState $ \st -> st { lblCount = rval + 1 }
  return (show rval)

uniqueLabel = do
  rval <- uniqueNumber
  return $ AId ("__t" ++ rval)

globalLabel var = do
  uid <- uniqueNumber
  return $ globalPrefix ++ var ++ "_" ++ uid

paramLabel var = do
  uid <- uniqueNumber
  return $ paramPrefix ++ var ++ "_" ++ uid

localLabel var = do
  uid <- uniqueNumber
  return $ localPrefix ++ var ++ "_" ++ uid

methodLabel var = do
  uid <- uniqueNumber
  return $ methodPrefix ++ var ++ "_" ++ uid

labelLabel = do
  uid <- uniqueNumber
  return $ AId (labelPrefix ++ uid)

data Instr =
  IAlloc Label                          | --Allocate a new variable
  IAllocArr Label Operand               | --Allocate an array
  IAllocParam Label Int MAlice.Type     | --Allocate a function param
  IAssignB Label Operand String Operand | --x := y `op` z
  IAssignU Label String Operand         | --x := op y
  ICopy Label Label                     | --x := y
  ICall Label Int                       | --x := y(..) with n params
  IGoto Label                           | --goto Label
  ICGoto Label Label                    | --if a goto label
  INCGoto Label Label                   | --if !a goto label
  ILabel Label                          | --Label:
  IMLabel Label (Maybe MAlice.Type)     | --Method label
  IParam Label                          | --Use label as param in call
  IReturn Label                         | --Return label
  IPrint Label                          | --print label
  IExit                                 |
  IInput Label
  deriving (Eq, Show)

type Label = Operand

data Operand =
  AId String              | --id/label
  AString String          | --String literal
  AInt Int                | --x := Int literal
  AChar Char              | --x := Char literal
  AArrRef Label Operand   | --a[ix]
  ACall Label Int           --f() with x params
  deriving (Eq, Show)
