module MAlice.SemanticAnalysis.TypeCheck where

typecheck :: Program -> IO ()
typecheck (Program ds) = typecheckDecls ds

typecheckDecls :: Decls -> IO ()
typecheckDecls (DeclList ds) = mapM_ typecheckDecl ds

typecheckDecl :: Decl -> IO ()
typecheckDecl (VarDecl _ _) =
  return ()
typecheckDecl (VAssignDecl t _ expr) =
  if (t == exprType expr) then return () else error "Assignment type error"
typecheckDecl (VArrayDecl _ _ expr) =
  if (exprType expr == Number) then return () else error "Invalid array index"
typecheckDecl (FuncDecl _ _ t body) =
  checkReturnType t body >> typecheckBody body
typecheckDecl (ProcDecl _ _ body) =
  typecheckBody body

checkReturnType :: Type -> Body -> IO ()
checkReturnType = undefined

typecheckBody :: Body -> IO ()
typecheckBody (DeclBody ds cst) =
  typecheckDecls ds >> typecheckCstmt cst
typecheckBody (StmtBody cst) =
  typecheckCstmt cst
typecheckBody _ =
  return ()
