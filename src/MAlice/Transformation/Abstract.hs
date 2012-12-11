module MAlice.Transformation.Abstract
       (abstract)
       where

import Control.Monad.State
import Data.Maybe
import qualified Data.Map as M
import MAlice.Language.AST
import MAlice.Transformation.Types

type ArgTable = M.Map String FreeVars

type Abs a = State ArgTable a

-- putFunction = flip flip put . ((>>=) .) . flip flip get . M.insert
putFunction :: String -> FreeVars -> Abs ()
putFunction k e =
  M.insert k e `liftM` get >>= put

getFunction :: String -> Abs FreeVars
getFunction k =
  (fromJust . M.lookup k) `liftM` get

abstract :: ADecls -> Program
abstract ad = Program $ evalState (abstractDecls ad) M.empty

abstractDecls :: ADecls -> Abs Decls
abstractDecls = undefined
