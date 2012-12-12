module MAlice.Transformation.Types where

import qualified MAlice.Language.Types as MAlice
import MAlice.Language.AST as AST
import MAlice.Language.Types

import Data.Maybe
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State

type Transform a = StateT TState Identity a

data SymbolTableEntry = STE
                        { alias :: String
                        , vType :: Type }
                        deriving (Eq, Show)


type SymbolTable = M.Map String SymbolTableEntry

data TState = TState { lblCount  :: Int
                     , symTables :: [SymbolTable] }

initState = TState { lblCount = 0
                   , symTables = [M.empty] }

type FreeVar = (String, Type)
type FreeVars = [FreeVar]

type ADecls = [ADecl]

data ADecl =
  AVarDecl Type Ident                              |
  AVAssignDecl Type Ident AExpr           FreeVars |
  AVArrayDecl Type Ident AExpr            FreeVars |
  AFuncDecl Ident FormalParams Type ABody FreeVars |
  AProcDecl Ident FormalParams ABody      FreeVars
  deriving (Eq, Show)

data ABody =
  AEmptyBody                              |
  AStmtBody ACompoundStmt        FreeVars |
  ADeclBody ADecls ACompoundStmt FreeVars
  deriving (Eq, Show)

data ACompoundStmt =
  ACSList [AStmt]
  deriving (Eq, Show)

data AStmt =
  ASBody ABody               FreeVars |
  ASNull                              |
  ASAssign AExpr AExpr       FreeVars |
  ASInc AExpr                FreeVars |
  ASDec AExpr                FreeVars |
  ASReturn AExpr             FreeVars |
  ASPrint AExpr              FreeVars |
  ASInput AExpr              FreeVars |
  ASCall Ident AActualParams FreeVars |
  ASLoop AExpr ACompoundStmt FreeVars |
  ASIf [AIfClause]           FreeVars
  deriving (Eq, Show)

data AIfClause =
  AIf AExpr ACompoundStmt FreeVars |
  AElse ACompoundStmt     FreeVars
  deriving (Eq, Show)

data AExpr =
  AEBinOp String AExpr AExpr      FreeVars |
  AEUnOp String AExpr             FreeVars |
  AEId Type Ident                 FreeVars |
  AEString String                          |
  AEInt IntLiteral                         |
  AEChar Char                              |
  AEArrRef Type Ident AExpr       FreeVars |
  AEBool Bool                              |
  AECall Type Ident AActualParams FreeVars
  deriving (Eq, Show)

data AActualParams =
  AAPList [AExpr]
  deriving (Eq, Show)


getSymbolTables :: Transform [SymbolTable]
getSymbolTables =
  symTables `liftM` get

insertSymbol :: String -> String -> Type -> Transform ()
insertSymbol k e typ = do
  (t:ts) <- getSymbolTables
  let ste = STE e typ
      newt = M.insert k ste t
  updateState $ \st -> st { symTables = newt : ts }

newSymbolTable :: Transform ()
newSymbolTable = do
  ts <- getSymbolTables
  updateState $ \st -> st { symTables = M.empty : ts }

removeSymbolTable :: Transform ()
removeSymbolTable = do
  (t : ts) <- getSymbolTables
  updateState $ \st -> st { symTables = ts }

updateState :: (TState -> TState) -> Transform ()
updateState f = do
  st <- get
  put $ f st

getDefinition :: String -> Transform String
getDefinition v = (fromJust . lookupInTables v) `liftM` getSymbolTables

lookupInTables :: String -> [SymbolTable] -> Maybe String
lookupInTables _ [] = Nothing
lookupInTables s (t : ts) =
  case M.lookup s t of
    Nothing -> lookupInTables s ts
    Just e -> Just (alias e)

enterBlock (FPList ps) = do
  newSymbolTable
  mapM_ (\(Param t var) -> insertSymbol var var t) ps

exitBlock = removeSymbolTable

isFreeVariable :: String -> Transform Bool
isFreeVariable var = do
  ts <- getSymbolTables
  let notGlobal = M.lookup var (last ts) == Nothing
      notLocal  = M.lookup var (head ts) == Nothing
  return $ notGlobal && notLocal

globalPrefix = "__global_"
paramPrefix  = "__param_"
localPrefix  = "__var_"
labelPrefix  = "__label_"
methodPrefix = "_m_"

uniqueNumber = do
  rval <- lblCount `liftM` get
  updateState $ \st -> st { lblCount = rval + 1 }
  return (show rval)

globalLabel var =
  return $ globalPrefix ++ var

paramLabel var = do
  uid <- uniqueNumber
  return $ paramPrefix ++ var ++ "_" ++ uid

localLabel var = do
  uid <- uniqueNumber
  return $ localPrefix ++ var ++ "_" ++ uid

methodLabel :: String -> Transform String
methodLabel var = do
  uid <- uniqueNumber
  return $ methodPrefix ++ var ++ "_" ++ uid

labelLabel = do
  uid <- uniqueNumber
  return $ labelPrefix ++ uid