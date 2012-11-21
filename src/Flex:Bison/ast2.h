#ifndef AST_H
#define AST_H
#include <vector>
#include <string>

class Declarations;
class Declaration;
class VarDecl;
class FuncDecl;
class ProcDecl;
class FormalParams;
class FormalParam;
class Body;
class Statement;
class Expression;
class CondStmt;
class ActualParams;

class Declarations {
public:
     std::vector<Declaration> decList;
};

class Declaration {
public:
     std::string id;
     Declaration(std::string _id)
          : id(_id) { }
};

class VarDecl : public Declaration {
public:
     int type;
     Expression *expr;
     VarDecl(std::string _id, int _type)
          : Declaration(_id), type(_type) { }
     VarDecl(std::string _id, int _type, Expression *_expr)
          : Declaration(_id), type(_type), expr(_expr) { }
};

class FuncDecl : public Declaration {
public:
     FormalParams *formalParams;
     int type;
     Body *body;
     FuncDecl(std::string _id, FormalParams *_formalParams, int _type, Body *_body)
          : Declaration(_id), formalParams(_formalParams), type(_type), body(_body) { }
};

class ProcDecl : public Declaration {
public:
     FormalParams *formalParams;
     int type;
     ProcDecl(std::string _id, FormalParams *_formalParams, int _type)
          : Declaration(_id), formalParams(_formalParams), type(_type) { }
};

class FormalParams {
public:
     std::vector<FormalParam> formalParamList;
     FormalParams(std::vector<FormalParam> _fplist)
          : formalParamList(_fplist) { }
};

class FormalParam {
public:
     int type;
     std::string id;
     FormalParam(int _type, std::string _id)
          : type(_type), id(_id) { }
};

class Body {
public:
     std::vector<Declaration> decList;
     std::vector<Statement> statList;
     Body(std::vector<Declaration> _decList, std::vector<Statement> _statList)
          : decList(_decList), statList(_statList) { }
};

class Statement {
};

class CondStmt {
};

class Expression {
};

class ActualParams {
     std::vector<Expression> exprList;
};
#endif
