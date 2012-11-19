%{
#include "ast2.h"
extern int yylex();
void yyerror(const char *s) { }
Declerations *programStart;
%}
%code requires {
#include "ast2.h"
}
%union {
     VarDecl *varDecl;
     FuncDecl *funcDecl;
     ProcDecl *procDecl;
     Declerations *decls;
     Decleration *decl;
     FormalParams *formalParams;
     FormalParam *formalParam;
     Body *body;
     Statement *stmt;
     Expression *expr;
     CondStmt *condStmt;
     ActualParams *actualParams;
     std::vector<Decleration*> *decList;
     std::vector<FormalParam*> *formalParamList;
     std::vector<Statement*> *statList;
     std::vector<Expression*> *exprList;
     std::string *string;
     int token;
}

%token <token> A AND ALICE ATE BECAME BECAUSE BUT CLOSED CONTAINED DRANK
%token <token> EITHER ENOUGH EVENTUALLY FOUND GLASS HAD LETTER LOOKING
%token <token> MAYBE NUMBER OF OR OPENED PERHAPS PIECE ROOM S SAID
%token <token> SENTENCE SO SPIDER SPOKE THE THEN TIMES TOO UNSURE WAS
%token <token> WHAT WHICH APOSTROPHE LOGICAL_OR LOGICAL_AND NOT_EQUAL
%token <token> NOT EQUAL LESS_EQUAL GREATER_EQUAL LESS_THAN GREATER_THAN
%token <token> COMMA FULLSTOP MULTIPLY ADD MINUS DIV MOD L_BRACKET R_BRACKET
%token <token> L_A_BRACKET R_A_BRACKET Q_MARK LOGICAL_NOT BITWISE_OR BITWISE_AND
%token <token> BITWISE_XOR BITWISE_NOT DIGIT
%token <token> SPACE TAB NEWLINE ERROR STOP
%token <string> ID STRING_LITERAL CHAR_LITERAL INT_LITERAL


%type <decl> decl varDecl funcDecl procDecl
%type <decls> program decls
%type <expr> expr

%start program

%right in
%left GREATER_THAN GREATER_EQUAL LESS_THAN LESS_EQUAL
%left LOGICAL_OR
%left LOGICAL_AND
%left BITWISE_OR
%left BITWISE_XOR
%left BITWISE_AND
%left EQUAL NOT_EQUAL

%left ADD MINUS
%left MULTIPLY DIV MOD
%right LOGICAL_NOT BITWISE_NOT UMINUS UPLUS
%right L_BRACKET

%%

program 	: 	decls { }
decls 		:	decls decl { } 
			| decl { }
decl		:	varDecl { } 
			| funcDecl { }  
			| procDecl { }
varDecl		:	ID WAS A type terminator {  }
			| ID WAS A type TOO terminator { }
varDecl		:	ID WAS A type OF expr terminator {  }
varDecl		:	ID HAD expr type terminator { } 
funcDecl	:	THE ROOM ID formalParams CONTAINED A type body { }
procDecl	:	THE LOOKING MINUS GLASS ID formalParams body { }
formalParams	:	L_BRACKET R_BRACKET { }
formalParams	:	L_BRACKET formalParamsList R_BRACKET { }
formalParamsList:	formalParam { } 
			| formalParamsList COMMA formalParam { }
formalParam	:	type ID { }
formalParam	:	refType ID { }
body		:	OPENED decls compoundStmt CLOSED { } 
			| OPENED compoundStmt CLOSED { } 
			| OPENED CLOSED { }
compoundStmt	:	compoundStmt stmt { }  | stmt { }
stmt		:	body { }
stmt		: 	expr BECAME expr terminator { }
stmt		:	FULLSTOP { }
stmt		:	expr ATE terminator { }
stmt		:	expr DRANK terminator { }
stmt		:	ALICE FOUND expr FULLSTOP { }
stmt		:	expr SPOKE terminator { } | expr SAID ALICE terminator { }
stmt		:	WHAT WAS expr Q_MARK { }
stmt		:	ID actualParams terminator { }
stmt		:	EVENTUALLY L_BRACKET expr R_BRACKET BECAUSE compoundStmt ENOUGH TIMES { }
stmt		:	EITHER L_BRACKET expr R_BRACKET SO compoundStmt OR compoundStmt BECAUSE ALICE WAS
			UNSURE WHICH { }
stmt		:	conditionalStmt BECAUSE ALICE WAS UNSURE WHICH { }
			| conditionalStmt OR compoundStmt BECAUSE ALICE WAS UNSURE WHICH { }
conditionalStmt :	PERHAPS L_BRACKET expr R_BRACKET SO compoundStmt { }
			| conditionalStmt OR MAYBE L_BRACKET expr R_BRACKET SO compoundStmt { }
type		:	NUMBER { }
type		:	LETTER { }
type		:	SENTENCE { }
refType		:	SPIDER type { }
expr		:	LOGICAL_NOT expr { } 
			| BITWISE_NOT expr { } 
			| ADD expr %prec UPLUS { } 
			| MINUS expr %prec UMINUS { }
expr		:	ID { }
expr		:	ID APOSTROPHE S expr PIECE { }
expr		:	STRING_LITERAL { }
expr		:	CHAR_LITERAL { }
expr		:	INT_LITERAL { }
expr		:	L_BRACKET expr R_BRACKET { }
expr		:	expr ADD expr { }
			| expr MINUS expr { }
			| expr MULTIPLY expr { }
			| expr DIV expr { }
			| expr MOD expr { }
			| expr BITWISE_AND expr { }
			| expr BITWISE_OR expr { }
			| expr BITWISE_XOR expr { }
			| expr LOGICAL_OR expr { }
			| expr LOGICAL_AND expr { }
			| expr GREATER_THAN expr { }
			| expr LESS_THAN expr { }
			| expr GREATER_EQUAL expr { }
			| expr LESS_EQUAL expr { }
			| expr EQUAL expr { }
			| expr NOT_EQUAL expr { }
actualParams	:	L_BRACKET actualParamsList R_BRACKET { }
actualParams	:	L_BRACKET R_BRACKET { }
actualParamsList:	expr  { }
			| actualParamsList COMMA expr { }
terminator	:	FULLSTOP { } | COMMA { } | AND { } | BUT { } | THEN { }

%%
