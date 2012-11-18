{
module Main where
import Scanner
}

%name parser
%tokentype { Lexeme }
%error { parseError }

%token
  "a"                   { L _ ( TKeyword "a" ) }
  "Alice"               { L _ ( TKeyword "Alice" ) }
  "and"                 { L _ ( TDelimiter "and" ) }
  "ate"                 { L _ ( TKeyword "ate" ) }
  "became"              { L _ ( TKeyword "became" ) }
  "because"             { L _ ( TKeyword "because" ) }
  "but"                 { L _ ( TDelimiter "but" ) }
  "closed"              { L _ ( TKeyword "closed" ) }
  "contained"           { L _ ( TKeyword "contained" ) }
  "drank"               { L _ ( TKeyword "drank" ) }
  "either"              { L _ ( TKeyword "either" ) }
  "enough"              { L _ ( TKeyword "enough" ) }
  "eventually"          { L _ ( TKeyword "eventually" ) }
  "found"               { L _ ( TKeyword "found" ) }
  "glass"               { L _ ( TKeyword "glass" ) }
  "had"                 { L _ ( TKeyword "had" ) }
  "letter"              { L _ ( TKeyword "letter" ) }
  "looking"             { L _ ( TKeyword "looking" ) }
  "maybe"               { L _ ( TKeyword "maybe" ) }
  "number"              { L _ ( TKeyword "number" ) }
  "of"                  { L _ ( TKeyword "of" ) }
  "or"                  { L _ ( TKeyword "or" ) }
  "opened"              { L _ ( TKeyword "opened" ) }
  "perhaps"             { L _ ( TKeyword "perhaps" ) }
  "piece"               { L _ ( TKeyword "piece" ) }
  "room"                { L _ ( TKeyword "room" ) }
  "s"                   { L _ ( TKeyword "s" ) }
  "said"                { L _ ( TKeyword "said" ) }
  "sentence"            { L _ ( TKeyword "sentence" ) }
  "so"                  { L _ ( TKeyword "so" ) }
  "spider"              { L _ ( TKeyword "spider" ) }
  "spoke"               { L _ ( TKeyword "spoke" ) }
  "The"                 { L _ ( TKeyword "The" ) }
  "then"                { L _ ( TDelimiter "then" ) }
  "times"               { L _ ( TKeyword "times" ) }
  "too"                 { L _ ( TKeyword "too" ) }
  "unsure"              { L _ ( TKeyword "unsure" ) }
  "was"                 { L _ ( TKeyword "was" ) }
  "what"                { L _ ( TKeyword "what" ) }
  "which"               { L _ ( TKeyword "which" ) }
  int_literal           { L _ ( TInt $$ ) }
  "."                   { L _ ( TDelimiter "." ) }
  "&&"                  { L _ ( TOperator "&&" ) }
  "||"                  { L _ ( TOperator "||" ) }
  "!"                   { L _ ( TOperator "!" ) }
  "~"                   { L _ ( TOperator "~" ) }
  "+"                   { L _ ( TOperator "+" ) }
  "-"                   { L _ ( TOperator "-" ) }
  "*"                   { L _ ( TOperator "*" ) }
  "/"                   { L _ ( TOperator "/" ) }
  "<"                   { L _ ( TOperator "<" ) }
  "<="                  { L _ ( TOperator "<=" ) }
  ">"                   { L _ ( TOperator ">" ) }
  ">="                  { L _ ( TOperator ">=" ) }
  "!="                  { L _ ( TOperator "!=" ) }
  "%"                   { L _ ( TOperator "%" ) }
  "=="                  { L _ ( TOperator "==" ) }
  "("                   { L _ ( TSymbol "(" ) }
  ")"                   { L _ ( TSymbol ")" ) }
  ident                 { L _ ( TIdent $$ ) }
  string_literal        { L _ ( TString $$ ) }
  char_literal          { L _ ( TChar $$ ) }
  ","                   { L _ ( TDelimiter "," ) }
  "?"                   { L _ ( TSymbol "?" ) }
  "^"                   { L _ ( TOperator "^" ) }
  "|"                   { L _ ( TOperator "|" ) }
  "&"                   { L _ ( TOperator "&" ) }
  "'"                   { L _ ( TSymbol "'" ) }

%right in
%left ">" ">=" "<" "<="
%left "||"
%left "&&"
%left "|"
%left "^"
%left "&"
%left "==" "!="

%left "+" "-"
%left "*" "/" "%"
%right "!" "~" UMINUS UPLUS
%right "("
%%

Root:
  Decls { CompilationUnit $1 }

Decls:
  Decls Decl { Decls $1 $2 } |
  Decl { SingleDecl $1 }

Decl:
  ident "was" "a" Type Terminator { VarDecl $4 $1 } |
  ident "was" "a" Type "too" Terminator { VarDecl $4 $1 } |
  ident "was" "a" Type "of" Expr Terminator { VAssignDecl $4 $1 $6 } |
  ident "had" Expr Type Terminator { VArrayDecl $4 $1 $3 } |
  "The" "room" ident FormalParams "contained" "a" Type Body { FuncDecl $3 $4 $7 $8 } |
  "The" "looking" "-" "glass" ident FormalParams Body { ProcDecl $5 $6 $7 }

FormalParams:
  "(" ")" { FVoidParam } |
  "(" FormalParamsList ")" { FPList $2 }

FormalParamsList:
  FormalParam { SingleParam $1 } |
  FormalParamsList "," FormalParam { FormalParamsList $1 $3 }

FormalParam:
  Type ident { Param $1 $2 } |
  RefType ident { Param $1 $2 }

Body:
  "opened" Decls CompoundStmt "closed" { DeclBody $2 $3 } |
  "opened" CompoundStmt "closed" { StmtBody $2 } |
  "opened" "closed" { EmptyBody }

CompoundStmt:
  CompoundStmt Stmt { CompoundStmt $1 $2 } |
  Stmt { SingleStmt $1 }

Stmt:
  Body { SBody $1 }|
  "." { SNull }|
  Expr "became" Expr Terminator { SAssign $1 $3 } |

  Expr "ate" Terminator { SInc $1 } |
  Expr "drank" Terminator { SDec $1 } |
  "Alice" "found" Expr "." { SReturn $3 } |
  Expr "said" "Alice" Terminator { SPrint $1 } |
  Expr "spoke" Terminator { SPrint $1 } |
  "what" "was" Expr "?" { SInput $3 } |
  ident ActualParams Terminator { SCall $1 $2 } |
  "eventually" "(" Expr ")" "because" CompoundStmt "enough" "times" { SLoop $3 $6 } |
  "either" "(" Expr ")" "so" CompoundStmt "or" CompoundStmt "because" "Alice" "was" "unsure" "which" { SIf $3 $6 $8 } |
  ConditionalStmt "because" "Alice" "was" "unsure" "which" { SEndC $1 } |
  ConditionalStmt "or" CompoundStmt "because" "Alice" "was" "unsure" "which" { SCond $1 $3 }

ConditionalStmt:
  "perhaps" "(" Expr ")" "so" CompoundStmt { CPerhaps $3 $6 } |
  ConditionalStmt "or" "maybe" "(" Expr ")" "so" CompoundStmt { COrMaybe $1 $5 $8 }

Type:
  "number" { Number } |
  "sentence" { Sentence } |
  "letter" { Letter }

RefType:
  "spider" Type { RefType $2 }

Expr:
  Expr "+" Expr  { EPlus $1 $3 } |
  Expr "-" Expr  { EMinus $1 $3 } |
  Expr "*" Expr  { EMult $1 $3 } |
  Expr "/" Expr  { EDiv $1 $3 } |
  Expr "%" Expr  { EMod $1 $3 } |
  Expr "&" Expr  { EBAnd $1 $3 } |
  Expr "|" Expr  { EBOr $1 $3 } |
  Expr "^" Expr  { EBXor $1 $3 } |
  Expr "||" Expr { ELOr $1 $3 } |
  Expr "&&" Expr { ELAnd $1 $3 } |
  Expr ">" Expr  { EGT $1 $3 } |
  Expr ">=" Expr { EGTE $1 $3 } |
  Expr "==" Expr { EEq $1 $3 } |
  Expr "<=" Expr { ELTE $1 $3 } |
  Expr "<" Expr  { ELT $1 $3 } |
  Expr "!=" Expr { ENEq $1 $3 } |
  "!" Expr { ENot $2 } |
  "~" Expr { EInv $2 } |
  "-" Expr %prec UMINUS { ENegate $2 } |
  "+" Expr %prec UPLUS { EPositive $2 } |
  ident { EId $1 } |
  string_literal { EString $1 } |
  int_literal { EInt $1 } |
  char_literal { EChar $1 } |
  ident "'" "s" Expr "piece" { EArrRef $1 $4 } |
  "(" Expr ")" { EBkt $2 } |
  ident ActualParams { ECall $1 $2 }

ActualParams:
  "(" ActualParamsList ")" { ActualParams $2 } |
  "(" ")" { Void }

ActualParamsList:
  Expr { SingleExpr $1 } |
  ActualParamsList "," Expr { ActualParamsList $1 $3 }

Terminator:
  "." {} |
  "," {} |
  "and" {} |
  "but" {} |
  "then" {}

{
parseError :: [Lexeme] -> a
parseError ((L pos t):ls) = error $ "Parse error at (" ++ showPosn pos ++ "):\n" ++ show t

data Program =
  CompilationUnit Decls
  deriving (Eq)

instance Show Program where
  show (CompilationUnit decls) =
    show decls

data Decls =
  Decls Decls Decl | SingleDecl Decl
  deriving (Eq)

instance Show Decls where
  show (Decls decls decl) =
    show decl ++ show decls
  show (SingleDecl decl) =
    show decl

data Decl =
  VarDecl Type Ident |
  VAssignDecl Type Ident Expr |
  VArrayDecl Type Ident Expr |
  FuncDecl Ident FormalParams Type Body |
  ProcDecl Ident FormalParams Body
  deriving (Eq)

instance Show Decl where
  show (VarDecl vtype ident) =
    show vtype ++ " " ++ ident ++ ";\n"
  show (VAssignDecl vtype ident expr) =
    show vtype ++ " " ++ ident ++ " = " ++ show expr ++ ";\n"
  show (VArrayDecl atype ident expr) =
    show atype ++ "[" ++ show expr ++ "] " ++ show ident ++ ";\n"
  show (FuncDecl ident params ftype body) =
    show ftype ++ " " ++ ident ++ "(" ++ show params ++ ") {\n" ++
    show body ++ "\n}"
  show (ProcDecl ident params body) =
    "void " ++ ident ++ "(" ++ show params ++ ") {\n" ++ show body ++ "}\n"

data FormalParams =
  FVoidParam | FPList FormalParamsList
  deriving (Eq)

instance Show FormalParams where
  show (FVoidParam) =
    "void"
  show (FPList fplist) =
    show fplist

data FormalParamsList =
  FormalParamsList FormalParamsList FormalParam | SingleParam FormalParam
  deriving (Eq)

instance Show FormalParamsList where
  show (SingleParam fp) =
    show fp
  show (FormalParamsList fplist fp) =
    show fp ++ ", " ++ show fplist

data FormalParam =
  Param Type Ident
  deriving (Eq)

instance Show FormalParam where
  show (Param ptype ident) =
    show ptype ++ " " ++ ident

data Body =
  DeclBody Decls CompoundStmt | StmtBody CompoundStmt | EmptyBody
  deriving (Eq)

instance Show Body where
  show (DeclBody decls cstmt) =
    show decls ++ show cstmt
  show (StmtBody cstmt) =
    show cstmt
  show EmptyBody =
    ""

data CompoundStmt =
  CompoundStmt CompoundStmt Stmt | SingleStmt Stmt
  deriving (Eq)

instance Show CompoundStmt where
  show (CompoundStmt cstmt stmt) =
    show cstmt ++ show stmt
  show (SingleStmt stmt) =
    show stmt

data Stmt =
  SBody Body |
  SNull |
  SAssign Expr Expr |
  SInc Expr |
  SDec Expr |
  SReturn Expr |
  SPrint Expr |
  SInput Expr |
  SCall Ident ActualParams |
  SLoop Expr CompoundStmt |
  SIf Expr CompoundStmt CompoundStmt |
  SEndC ConditionalStmt |
  SCond ConditionalStmt CompoundStmt
  deriving (Eq)

instance Show Stmt where
  show (SBody b) =
    show b
  show (SNull) =
    ""
  show (SAssign expr1 expr2) =
    show expr1 ++ " = " ++ show expr2 ++ ";\n"
  show (SInc expr) =
    show expr ++ "++;\n"
  show (SDec expr) =
    show expr ++ "--;\n"
  show (SReturn expr) =
    "return " ++ show expr ++ ";\n"
  show (SPrint expr) =
    "cout >> " ++ show expr ++ ";\n"
  show (SInput expr) =
    "cin << " ++ show expr ++ ";\n"
  show (SCall ident params) =
    ident ++ "(" ++ show params ++ ");\n"
  show (SLoop expr cstmt) =
    "while(" ++ show expr ++ ") {\n" ++
    show cstmt ++ "}\n"
  show (SIf expr c1 c2) =
    "if (" ++ show expr ++ ") {\n" ++ show c1 ++ "} else {\n" ++
    show c2 ++ "}\n"
  show (SEndC cond) =
    show cond
  show (SCond cond cstmt) =
    show cond ++ show cstmt

data ConditionalStmt =
  CPerhaps Expr CompoundStmt |
  COrMaybe ConditionalStmt Expr CompoundStmt
  deriving (Eq)

instance Show ConditionalStmt where
  show (CPerhaps expr cstmt) =
    "if (" ++ show expr ++ ") {\n" ++ show cstmt ++ "}\n"
  show (COrMaybe cond expr cstmt) =
    show cond ++ "else if (" ++ show expr ++ ") {\n" ++ show cstmt ++ "}\n"

data Type =
  Number | Letter | Sentence | RefType Type
  deriving (Eq)

instance Show Type where
  show (Number) = "int"
  show (Letter) = "char"
  show (Sentence) = "string"
  show (RefType reftype) = "*" ++ show reftype

data Expr =
  EPlus Expr Expr |
  EMinus Expr Expr |
  EMult Expr Expr |
  EDiv Expr Expr |
  EMod Expr Expr |
  EBAnd Expr Expr |
  EBOr Expr Expr |
  EBXor Expr Expr |
  ELOr Expr Expr |
  ELAnd Expr Expr |
  EGT Expr Expr |
  EGTE Expr Expr |
  EEq Expr Expr |
  ELTE Expr Expr |
  ELT Expr Expr |
  ENEq Expr Expr |
  ENot Expr |
  EInv Expr |
  EId Ident |
  EString String |
  EInt IntLiteral |
  EChar Char |
  EArrRef Ident Expr |
  EBkt Expr |
  ECall Ident ActualParams |
  ENegate Expr |
  EPositive Expr
  deriving (Eq)

instance Show Expr where
  show (EPlus e1 e2) = show e1 ++ " + " ++ show e2
  show (EMinus e1 e2) = show e1 ++ " - " ++ show e2
  show (EMult e1 e2) = show e1 ++ " * " ++ show e2
  show (EDiv e1 e2) = show e1 ++ " / " ++ show e2
  show (EMod e1 e2) = show e1 ++ " % " ++ show e2
  show (EBAnd e1 e2) = show e1 ++ " & " ++ show e2
  show (EBOr e1 e2) = show e1 ++ " | " ++ show e2
  show (EBXor e1 e2) = show e1 ++ " ^ " ++ show e2
  show (ELAnd e1 e2) = show e1 ++ " && " ++ show e2
  show (ELOr e1 e2) = show e1 ++ "||" ++ show e2
  show (EGT e1 e2) = show e1 ++ " > " ++ show e2
  show (EGTE e1 e2) = show e1 ++ " >= " ++ show e2
  show (EEq e1 e2) = show e1 ++ " == " ++ show e2
  show (ELTE e1 e2) = show e1 ++ " <= " ++ show e2
  show (ELT e1 e2) = show e1 ++ " < " ++ show e2
  show (ENEq e1 e2) = show e1 ++ " != " ++ show e2
  show (ENot e1) = "!" ++ show e1
  show (EInv e1) = "~" ++ show e1
  show (EId ident) = ident
  show (EString str) = show str
  show (EInt int) = show int
  show (EChar c) = show c
  show (EArrRef ident e1) = ident ++ "[" ++ show e1 ++ "]"
  show (EBkt e1) = "(" ++ show e1 ++ ")"
  show (ENegate e1) = "-(" ++ show e1 ++ ")"
  show (EPositive e1) = "+(" ++ show e1 ++ ")"
  show (ECall ident params) = ident ++ "(" ++ show params ++ ")"
data ActualParams =
  ActualParams ActualParamsList | Void
  deriving (Eq)

instance Show ActualParams where
  show (Void) = ""
  show (ActualParams list) =
    show list

data ActualParamsList =
  ActualParamsList ActualParamsList Expr | SingleExpr Expr
  deriving (Eq)

instance Show ActualParamsList where
  show (SingleExpr expr) =
    show expr
  show (ActualParamsList apl expr) =
    show expr ++ "," ++ show apl

type Ident = String
type IntLiteral = Int

main = do
  inStr <- getContents
  let parseTree = parser $ scanString inStr
  putStrLn $ "parseTree: " ++ show parseTree
  putStrLn $ "Done."

}
