{
module Parser where
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
  deriving (Eq, Show)

data Decls =
  Decls Decls Decl | SingleDecl Decl
  deriving (Eq, Show)

data Decl =
  VarDecl Type Ident |
  VAssignDecl Type Ident Expr |
  VArrayDecl Type Ident Expr |
  FuncDecl Ident FormalParams Type Body |
  ProcDecl Ident FormalParams Body
  deriving (Eq, Show)

data FormalParams =
  FVoidParam | FPList FormalParamsList
  deriving (Eq, Show)

data FormalParamsList =
  FormalParamsList FormalParamsList FormalParam | SingleParam FormalParam
  deriving (Eq, Show)

data FormalParam =
  Param Type Ident
  deriving (Eq, Show)

data Body =
  DeclBody Decls CompoundStmt | StmtBody CompoundStmt | EmptyBody
  deriving (Eq, Show)

data CompoundStmt =
  CompoundStmt CompoundStmt Stmt | SingleStmt Stmt
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data ConditionalStmt =
  CPerhaps Expr CompoundStmt |
  COrMaybe ConditionalStmt Expr CompoundStmt
  deriving (Eq, Show)

data Type =
  Number | Letter | Sentence | RefType Type
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data ActualParams =
  ActualParams ActualParamsList | Void
  deriving (Eq, Show)

data ActualParamsList =
  ActualParamsList ActualParamsList Expr | SingleExpr Expr
  deriving (Eq, Show)

type Ident = String
type IntLiteral = Int

}
