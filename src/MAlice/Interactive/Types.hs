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

data UserInput =
  EvalStmt CompoundStmt |
  EvalExpr Expr         |
  EvalDecl Decl         |
  Command UserCommand
  deriving (Eq, Show)

data UserCommand =
  LoadFile String |
  ReloadFile      |
  Quit            |
  Help
  deriving (Eq, Show)

type VarTable =
  M.Map String Var

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

type MEval = StateT RuntimeState IO
type MExec a = ErrorT String MEval a

runMExec :: MExec a -> MEval (Either String a)
runMExec = runErrorT

data RuntimeState = RuntimeState
                    { parserState  :: ParserState
                    , programAST   :: Program
                    , memory       :: [VarTable]
                    , currentFile  :: String }

getParserState :: MEval ParserState
getParserState = parserState `liftM` get

getCurrentFile :: MEval String
getCurrentFile = currentFile `liftM` get

modifyState :: (RuntimeState -> RuntimeState) -> MEval ()
modifyState f = get >>= return . f >>= put

lookupVarTable :: String -> VarTable -> Maybe Var
lookupVarTable s vt = M.lookup s vt

lookupVarTables :: String -> [VarTable] -> Maybe Var
lookupVarTables _ [] = Nothing
lookupVarTables s (t : ts) =
  case lookupVarTable s t of
    Nothing -> lookupVarTables s ts
    e       -> e

getVarTables :: MEval [VarTable]
getVarTables = memory `liftM` get

addVar :: String -> Var -> MEval ()
addVar i v = do
  (t : ts) <- getVarTables
  let nt = M.insert i v t
  modifyState $ \st -> st { memory = nt : ts }

setVar :: String -> Var -> MEval ()
setVar = addVar

getVar :: String -> MEval Var
getVar v = do
  ts <- getVarTables
  let (Just var) = lookupVarTables v ts
  return var
--crash on retrieving Nothing!!

getArrElem :: String -> Int -> MEval Var
getArrElem a ix = do
  (ArrVar array) <- getVar a
  return $ array V.! ix

newVar :: Type -> String -> MEval ()
newVar t i = do
  let val = emptyVal t
  addVar i val

newArray :: Type -> String -> Int -> MEval ()
newArray t i n = do
  let val = V.replicate n (emptyVal t)
  addVar i (ArrVar val)

emptyVal :: Type -> Var
emptyVal t =
  case t of
    Number   -> IntVar Nothing
    Sentence -> StrVar Nothing
    Letter   -> ChrVar Nothing
    Boolean  -> BolVar Nothing

newDecl :: String -> Decl -> MEval ()
newDecl i d = addVar i (MDecl d)

enterMethod :: [Var] -> FormalParams -> MEval ()
enterMethod vs (FPList fps) = do
  modifyState $ \st -> st { memory = M.empty : memory st }
  let vfs = zip vs fps
  mapM_ (\(var, (Param _ i)) -> setVar i var) vfs

exitMethod :: MEval ()
exitMethod = modifyState $ \st -> st { memory = tail . memory $ st }

enterBlock :: MEval ()
enterBlock = modifyState $ \st -> st { memory = M.empty : memory st }

exitBlock = exitMethod