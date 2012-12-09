module MAlice.Transformation.Types where

import qualified MAlice.Language.Types as MAlice
import qualified MAlice.Language.AST as AST
import MAlice.Language.Types

import Data.Maybe
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State

type Transform a = StateT TState Identity a
type SymbolTable = M.Map String String

data TState = TState { lblCount :: Int
                     , symTables :: [SymbolTable] }

initState = TState { lblCount = 0
                   , symTables = [M.empty] }

getSymbolTables :: Transform [SymbolTable]
getSymbolTables =
  symTables `liftM` get

insertSymbol :: String -> String -> Transform ()
insertSymbol k e = do
  (t:ts) <- getSymbolTables
  let newt = M.insert k e t
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
  return $ labelPrefix ++ uid