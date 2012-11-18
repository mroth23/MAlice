#ifndef AST_H
#define AST_H
#include <vector>
#include <string>

class Declerations;
class Decleration;
class VarDecl;
class FuncDecl;
class ProcDecl;
class FormalParams;
class FormalParam;
class Body;
class Statement;
class Expression;



class Declerations {
	public:
		std::vector<Decleration> decList;
};

class Decleration {
	public:
		std::string id;
};

class VarDecl : public Decleration {
	public:
		int type;
		Expression *expr;
		VarDecl(std::string _id, int _type) {
			id   = _id;
			type = _type;
		}
		VarDecl(std::string _id, int _type, Expression *_expr) {
			id   = _id;
			type = _type;
			expr = _expr;
		}
};

class FuncDecl : public Decleration {
	public:
		FormalParams *formalParams;
		int type;
		Body *body;
		FuncDecl(std::string _id, FormalParams *_formalParams, int _type, Body *_body) {
			id           = _id;
			formalParams = _formalParams;
			type         = _type;
			body         = _body;
		}
};

class ProcDecl : public Decleration {
	public:
		FormalParams *formalParams;
		int type;
		ProcDecl(std::string _id, FormalParams *_formalParams, int _type) {
			id           = _id;
			formalParams = _formalParams;
			type         = _type;
		}
};

class FormalParams {
	std::vector<FormalParam> formalParamList;
};

class FormalParam {
	int type;
	std::string id;
	FormalParam(int _type, std::string _id) {
		type = _type;
		id   = _id;
	}
};

class Body {
	std::vector<Decleration> decList;
	std::vector<Statement> statList;	
};

class Statment {
};

class CondStmt {
};

class Expression {
};

class ActualParams {
	std::vector<Expression> exprList;
};
#endif
