%{
	#include "parser.hpp"
	#include <string>
	#include <vector>
	#include "ast2.h"
	using namespace std;
	class Token {
		public:
			int token, line, character;
			string extra;
			Token (int _token, int _line, int _character) {
				token     = _token;
				line      = _line;
				character = _character;
			}
			Token (int _token, int _line, int _character, string _extra) {
				token     = _token;
				line      = _line;
				character = _character;
				extra     = _extra;
			}
	};
	int line = 1; 
	int character = 1;
	vector<Token*> tokens;
	void newToken (int token) {
		tokens.push_back(new Token(token, line, character));
		/*cout << "Line: " << line << ", Character: " << character << ", Token: " << token
		<< ", Token String: " << yytext  << endl;*/
		character += yyleng;
	}
	void newToken (int token, string extra) {
		tokens.push_back(new Token(token, line, character, extra));
		character += yyleng;
	}
	void throwError () {
		/*cout << "Lexical Error on line  " << line << " at character " << character
		<< " trying to tokenise '" << yytext <<  "'." << endl;*/
		printf("Lexical Error on line %i, character %i, trying to tokenise %s \n", line, character, yytext);
		exit(1);
		character += yyleng;
	} 
%}

%option noyywrap nounput

A		a
ALICE		Alice
AND		and
ATE		ate
BECAME		became
BECAUSE		because
BUT		but
CLOSED		closed
CONTAINED	contained
DRANK		drank
EITHER		either
ENOUGH		enough
EVENTUALLY	eventually
FOUND		found
GLASS		glass
HAD		had
LETTER		letter
LOOKING		looking
MAYBE		maybe
NUMBER		number
OF		of
OR		or
OPENED		opened
PERHAPS		perhaps
PIECE		piece
ROOM		room
S		s
SAID		said
SENTENCE	sentence
SO		so
SPIDER		spider
SPOKE		spoke
THE		The
THEN		then
TIMES		times
TOO		too
UNSURE		unsure
WAS		was
WHAT		what
WHICH		which

APOSTROPHE	'
LOGICAL_OR	\|\|
LOGICAL_AND	&&
NOT_EQUAL	!=
NOT		!
EQUAL		==
LESS_EQUAL	<=
GREATER_EQUAL	>=
LESS_THAN	<
GREATER_THAN	>

COMMA		,
FULLSTOP	\.
MULTIPLY	\*
MOD		%
ADD		\+
MINUS		\-
DIV		\/
L_BRACKET	\(
R_BRACKET	\)
R_A_BRACKET	\}
L_A_BRACKET	\{
Q_MARK		\?
BITWISE_OR	\|
BITWISE_AND	&
BITWISE_XOR	\^
BITWISE_NOT	~

STRING_LITERAL	\"(\\.|[^\"])*\"
CHAR_LITERAL	\'(\\.|[^\"])\'
INT_LITERAL	(\-)?[0-9]+
ID		[a-zA-Z][a-zA-Z0-9]*|[0-9][a-zA-Z]+[a-zA-Z0-9]*

%%

{A}		newToken(A); return A;
{ALICE}		newToken(ALICE); return ALICE;
{AND}		newToken(AND); return AND;
{ATE}		newToken(ATE); return ATE;
{BECAME}	newToken(BECAME); return BECAME;
{BECAUSE}	newToken(BECAUSE); return BECAUSE;
{BUT}		newToken(BUT); return BUT;
{CLOSED}	newToken(CLOSED); return CLOSED;
{CONTAINED}	newToken(CONTAINED); return CONTAINED;
{DRANK}		newToken(DRANK); return DRANK;
{EITHER}	newToken(EITHER); return EITHER;
{ENOUGH}	newToken(ENOUGH); return ENOUGH;
{EVENTUALLY}	newToken(EVENTUALLY); return EVENTUALLY;
{FOUND}		newToken(FOUND); return FOUND;
{GLASS}		newToken(GLASS); return GLASS;
{HAD}		newToken(HAD); return HAD;
{LETTER}	newToken(LETTER); return LETTER;
{LOOKING}	newToken(LOOKING); return LOOKING;
{MAYBE}		newToken(MAYBE); return MAYBE;
{NUMBER}	newToken(NUMBER); return NUMBER;
{OF}		newToken(OF); return OF;
{OR}		newToken(OR); return OR;
{OPENED}	newToken(OPENED); return OPENED;
{PERHAPS}	newToken(PERHAPS); return PERHAPS;
{PIECE}		newToken(PIECE); return PIECE;
{ROOM}		newToken(ROOM); return ROOM;
{S}		newToken(S); return S;
{SAID}		newToken(SAID); return SAID;
{SENTENCE}	newToken(SENTENCE); return SENTENCE;
{SO}		newToken(SO); return SO;
{SPIDER}	newToken(SPIDER); return SPIDER;
{SPOKE}		newToken(SPOKE); return SPOKE;
{THE}		newToken(THE); return THE;
{THEN}		newToken(THEN); return THEN;
{TIMES}		newToken(TIMES); return TIMES;
{TOO}		newToken(TOO); return TOO;
{UNSURE}	newToken(UNSURE); return UNSURE;
{WAS}		newToken(WAS); return WAS;
{WHAT}		newToken(WHAT); return WHAT;
{WHICH}		newToken(WHICH); return WHICH;

{APOSTROPHE}	newToken(APOSTROPHE); return APOSTROPHE;
{LOGICAL_OR}	newToken(LOGICAL_OR); return LOGICAL_OR;
{LOGICAL_AND}	newToken(LOGICAL_AND); return LOGICAL_AND;
{NOT_EQUAL}	newToken(NOT_EQUAL); return NOT_EQUAL;
{NOT}		newToken(NOT); return NOT;
{EQUAL}		newToken(EQUAL); return EQUAL;
{LESS_EQUAL}	newToken(LESS_EQUAL); return LESS_EQUAL;
{GREATER_EQUAL}	newToken(GREATER_EQUAL); return GREATER_EQUAL;
{LESS_THAN}	newToken(LESS_THAN); return LESS_THAN;
{GREATER_THAN}	newToken(GREATER_THAN); return GREATER_THAN;

{COMMA}		newToken(COMMA); return COMMA;
{FULLSTOP}	newToken(FULLSTOP); return FULLSTOP;
{MULTIPLY}	newToken(MULTIPLY); return MULTIPLY;
{ADD}		newToken(ADD); return ADD;
{MINUS}		newToken(MINUS); return MINUS;
{DIV}		newToken(DIV); return DIV;
{L_BRACKET}	newToken(L_BRACKET); return L_BRACKET;
{R_BRACKET}	newToken(R_BRACKET); return R_BRACKET;
{R_A_BRACKET}	newToken(R_A_BRACKET); return R_A_BRACKET;
{L_A_BRACKET}	newToken(L_A_BRACKET); return L_A_BRACKET;
{Q_MARK}	newToken(Q_MARK); return Q_MARK;
{BITWISE_OR}	newToken(BITWISE_OR); return BITWISE_OR;
{BITWISE_AND}	newToken(BITWISE_AND); return BITWISE_AND;
{BITWISE_XOR}	newToken(BITWISE_XOR); return BITWISE_XOR;
{BITWISE_NOT}	newToken(BITWISE_NOT); return BITWISE_NOT;

{STRING_LITERAL} newToken(STRING_LITERAL, (string) yytext); return STRING_LITERAL;
{CHAR_LITERAL}	newToken(CHAR_LITERAL, (string) yytext); return CHAR_LITERAL;
{INT_LITERAL}	newToken(INT_LITERAL, (string) yytext); return INT_LITERAL;
{ID}		newToken(ID, (string) yytext); return ID;

[ ]		character++;  		/*return SPACE; */
\t		character +=8;		/*return TAB;*/
\n		character = 1; line++; 	/*return NEWLINE;*/
<<EOF>>  	return 0;
.		throwError(); return 0;

%%
