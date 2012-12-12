module MAlice.Language.PrettyPrint
       ( pprint
       , pprintS
       ) where

-- This module pretty prints MAlice abstract syntax trees in a syntax similar
-- to C++.

import Data.List
import MAlice.Language.AST
import MAlice.Language.Types

pprint :: Program -> IO ()
pprint = putStrLn . pprintS

pprintS :: Program -> String
pprintS (Program ds) =
  ppDecls ds

ppDecls :: Decls -> String
ppDecls (DeclList ds) =
  unlines . map ppDecl $ ds

ppDecl :: Decl -> String
ppDecl (VarDecl vtype ident) =
  ppType vtype ++ " " ++ ident ++ ";"
ppDecl (VAssignDecl vtype ident expr) =
  ppType vtype ++ " " ++ ident ++ " = " ++ ppExpr expr ++ ";"
ppDecl (VArrayDecl atype ident expr) =
  ppType atype ++ "[" ++ ppExpr expr ++ "] " ++ ident ++ ";"
ppDecl (FuncDecl ident params ftype body) =
  ppType ftype ++ " " ++ ident ++ "(" ++ ppFPs params ++ ") {\n" ++
  ppBody body ++ "\n}\n"
ppDecl (ProcDecl ident params body) =
  "void " ++ ident ++ "(" ++ ppFPs params ++ ") {\n" ++ ppBody body ++ "\n}\n"

ppFPs :: FormalParams -> String
ppFPs (FPList fps) =
  foldr commaSeparate "" fps
  where
    commaSeparate f1 r =
      ppFP f1 ++ ", " ++ r

ppFP :: FormalParam -> String
ppFP (Param t var) = ppType t ++ " " ++ var

ppBody :: Body -> String
ppBody (EmptyBody) = ""
ppBody (StmtBody cst) = ppCompoundStmt cst
ppBody (DeclBody ds cst) = ppDecls ds ++ ppCompoundStmt cst

ppCompoundStmt :: CompoundStmt -> String
ppCompoundStmt (CSList ss)=
  unlines . map ppStmt $ ss

ppStmt :: Stmt -> String
ppStmt (SBody b) =
  "{\n" ++ ppBody b ++ "\n}"
ppStmt (SNull) =
  ""
ppStmt (SAssign expr1 expr2) =
  ppExpr expr1 ++ " = " ++ ppExpr expr2 ++ ";"
ppStmt (SInc expr) =
  ppExpr expr ++ "++;"
ppStmt (SDec expr) =
  ppExpr expr ++ "--;"
ppStmt (SReturn expr) =
  "return " ++ ppExpr expr ++ ";"
ppStmt (SPrint expr) =
  "cout << " ++ ppExpr expr ++ ";"
ppStmt (SInput expr) =
  "cin >> " ++ ppExpr expr ++ ";"
ppStmt (SCall ident params) =
  ident ++ "(" ++ ppAPs params ++ ");"
ppStmt (SLoop expr cstmt) =
  "while(" ++ ppExpr expr ++ ") {\n" ++
  ppCompoundStmt cstmt ++ "}"
ppStmt (SIf ifs) =
  ppIfs ifs

ppIfs :: [IfClause] -> String
ppIfs = concatMap ppIf

ppIf :: IfClause -> String
ppIf (If e c) =
  "if (" ++ ppExpr e ++ ") {\n" ++ ppCompoundStmt c ++ "\n} "
ppIf (Else c) =
  "else {\n" ++ ppCompoundStmt c ++ "\n}"

ppExpr :: Expr -> String
ppExpr (EBinOp op e1 e2)    = ppExpr e1 ++ " " ++ op ++ " " ++ ppExpr e2
ppExpr (EUnOp op e1)        = op ++ "(" ++ ppExpr e1 ++ ")"
ppExpr (EId _ ident)        = ident
ppExpr (EString str)        = show str
ppExpr (EInt int)           = show int
ppExpr (EChar c)            = show c
ppExpr (EBool b)            = show b
ppExpr (EArrRef _ ident e1) = ident ++ "[" ++ ppExpr e1 ++ "]"
ppExpr (ECall _ f aps)      = f ++ "(" ++ ppAPs aps ++ ")"

ppAPs :: ActualParams -> String
ppAPs (APList aps) =
  foldr commaSeparate "" aps
  where
    commaSeparate f1 r =
      ppExpr f1 ++ ", " ++ r

ppType :: Type -> String
ppType Number = "number"
ppType Letter = "letter"
ppType Sentence = "sentence"
ppType Boolean = "boolean"
ppType (RefType t) = "spider " ++ ppType t
ppType (Ref t) = "&" ++ ppType t