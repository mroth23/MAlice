module MAlice.Transformation.Collect where

import MAlice.Language.AST

-- This very simple module collects all function and method definitions
-- (they are now including all their free variables as arguments) and lifts
-- them in front of their parent function (recursively). Step 3/3 of the lambda
-- lifter.

collect :: Program -> Program
collect (Program ds) = Program $ collectDecls ds

collectDecls :: Decls -> Decls
collectDecls (DeclList ds) =
  DeclList $ concatMap collectDecl ds

collectDecl :: Decl -> [Decl]
collectDecl (FuncDecl f fps t b) =
  let (nb, ds) = collectBody b in
  concatMap collectDecl ds ++ [FuncDecl f fps t nb]
collectDecl (ProcDecl f fps b) =
  let (nb, ds) = collectBody b in
  concatMap collectDecl ds ++ [ProcDecl f fps nb]
collectDecl d = [d]

collectBody :: Body -> (Body, [Decl])
collectBody (DeclBody (DeclList ds) cst) =
  (DeclBody (DeclList decls) cst, lifted)
  where
    (lifted, decls) = collectSplit ds ([], [])
collectBody b = (b, [])

collectSplit :: [Decl] -> ([Decl], [Decl]) -> ([Decl], [Decl])
collectSplit [] acc = acc
collectSplit (d@(FuncDecl _ _ _ _) : ds) (lifted, decls) =
  collectSplit ds (lifted ++ [d], decls)
collectSplit (d@(ProcDecl _ _ _) : ds) (lifted, decls) =
  collectSplit ds (lifted ++ [d], decls)
collectSplit (d : ds) (lifted, decls) =
  collectSplit ds (lifted, decls ++ [d])
