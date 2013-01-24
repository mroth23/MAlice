module MAlice.Interactive.Types where

import qualified Data.Vector as V
import qualified Data.Map as M
import Data.Maybe
import Control.Monad
import Control.Monad.Error
import Control.Monad.State
import MAlice.Language.AST
import MAlice.Language.Types
import MAlice.Language.SymbolTable
import MAlice.Parser.ParserState

-- Different kinds of user input
data UserInput =
  EvalStmt CompoundStmt |
  EvalExpr Expr         |
  EvalDecl Decl         |
  Command UserCommand
  deriving (Eq, Show)

--Commands that the user can input
data UserCommand =
  LoadFile String |
  ReloadFile      |
  Quit            |
  Help
  deriving (Eq, Show)

--Stores values for identifiers in memory
type VarTable =
  M.Map String Var

--Different kinds of values that can be stored in memory
data Var =
  StrVar (Maybe String) |
  IntVar (Maybe Int)    |
  ChrVar (Maybe Char)   |
  BolVar (Maybe Bool)   |
  ArrVar (V.Vector Var) |
  MDecl  (Decl)
  deriving (Eq)

showM Nothing = "uninitialised variable"
showM (Just a) = show a
showMS Nothing = "uninitialised variable"
showMS (Just s) = s

instance Show Var where
  show (StrVar ms) = showMS ms
  show (IntVar mi) = showM mi
  show (ChrVar mc) = showM mc
  show (BolVar mb) = showM mb
  show (ArrVar v)  = show (V.toList v)

-- The monads that are used to evaluate MAlice programs
type MEval = StateT RuntimeState IO
-- Error handling strapped on MEval
type MExec a = ErrorT String MEval a

-- Unwraps an MEval computation from an MExec one
runMExec :: MExec a -> MEval (Either String a)
runMExec = runErrorT

-- The runtime state kept by the shell
data RuntimeState = RuntimeState
                    { parserState  :: ParserState
                    , programAST   :: Program
                    , memory       :: [VarTable]
                    , currentFile  :: String }

-- Returns parser state
getParserState :: MEval ParserState
getParserState = parserState `liftM` get

--Returns current file
getCurrentFile :: MEval String
getCurrentFile = currentFile `liftM` get

--Modifies the runtime state with the given function
modifyState :: (RuntimeState -> RuntimeState) -> MEval ()
modifyState f = get >>= return . f >>= put

--Lookup in one variable table (memory)
lookupVarTable :: String -> VarTable -> Maybe Var
lookupVarTable s vt = M.lookup s vt

--Global variable table lookup
lookupVarTables :: String -> [VarTable] -> Maybe Var
lookupVarTables _ [] = Nothing
lookupVarTables s (t : ts) =
  case lookupVarTable s t of
    Nothing -> lookupVarTables s ts
    e       -> e

--Returns all current var tables
getVarTables :: MEval [VarTable]
getVarTables = memory `liftM` get

--Add a new var into a table
addVar :: String -> Var -> MEval ()
addVar i v = do
  (t : ts) <- getVarTables
  let nt = M.insert i v t
  modifyState $ \st -> st { memory = nt : ts }

--Sets a variable to a value
setVar :: String -> Var -> MEval ()
setVar st val = do
  ts <- getVarTables
  let nts = insertIntoTables st val ts
  modifyState $ \st -> st { memory = nts }

insertIntoTables :: String -> Var -> [VarTable] -> [VarTable]
insertIntoTables st val [] = []
insertIntoTables st val (t:ts) =
  if M.member st t
  then (M.insert st val t) : ts
  else t : insertIntoTables st val ts

--Returns a variable value from memory
getVar :: String -> MEval Var
getVar v = do
  ts <- getVarTables
  --This is safe because failed lookups are caught by the type checker
  let (Just var) = lookupVarTables v ts
  return var

--Returns an array element value from memory
getArrElem :: String -> Int -> MExec Var
getArrElem a ix = do
  av <- lift $ getVar a
  case av of
    (ArrVar array) ->
      if ix > (V.length array) || ix < 0
        then throwError $
             "Eval.getArrElem: array index out of bounds, " ++ show ix
        else return $ array V.! ix
    _ -> throwError $ "Eval.getArrElem: not an array variable: " ++ a

--Creates a new variable in memory
newVar :: Type -> String -> MEval ()
newVar t i = do
  let val = emptyVal t
  addVar i val

--Creates a new array in memory
newArray :: Type -> String -> Int -> MEval ()
newArray t i n = do
  let val = V.replicate n (emptyVal t)
  addVar i (ArrVar val)

-- The empty (default) value for everything
emptyVal :: Type -> Var
emptyVal t =
  case t of
    Number   -> IntVar Nothing
    Sentence -> StrVar Nothing
    Letter   -> ChrVar Nothing
    Boolean  -> BolVar Nothing

--Adds a new declaration (function/procedure) to the variable table
newDecl :: String -> Decl -> MEval ()
newDecl i d = addVar i (MDecl d)

--Enter a new scope for evaluation
enterMethod :: [Var] -> FormalParams -> MEval ()
enterMethod vs (FPList fps) = do
  modifyState $ \st -> st { memory = M.empty : memory st }
  let vfs = zip vs fps
  mapM_ (\(var, (Param _ i)) -> setVar i var) vfs

--Exit scope
exitMethod :: MEval ()
exitMethod = modifyState $ \st -> st { memory = tail . memory $ st }

--Enter a block
enterBlock :: MEval ()
enterBlock = modifyState $ \st -> st { memory = M.empty : memory st }

--Exit a block
exitBlock = exitMethod