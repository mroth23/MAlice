module MAlice.Transformation.Desugar where

import MAlice.Language.AST
import MAlice.Transformation.Abstract
import MAlice.Transformation.Annotate
import MAlice.Transformation.Types

-- This module contains functions to "desugar" MAlice code. Most importantly,
-- inner functions are lifted to global level. This is done in three passes:
-- First, all function and procedure definitions are annotated with free
-- variables occurring in their bodies. All function calls are then changed
-- so the (formerly) free variables are bound by function arguments. Finally,
-- all the function definitions are collected and lifted from their original
-- position to an appropriate place in global scope.

desugarAST :: Program -> Program
desugarAST (Program ds) =
  Program $ desugarDecls ds

desugarDecls :: Decls -> Decls
desugarDecls (DeclList ds) =
  undefined
  --DeclList (collect . abstract . annotateDecls $ ds)

collect :: Decls -> Decls
collect = undefined

abstract :: ADecls -> Decls
abstract = undefined
