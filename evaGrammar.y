%{

#include <iostream>
#include <string>
#include <vector>

#include "evaParser.h"
#include "evaLexer.h"

#define YYDEBUG 1
#define YYERROR_VERBOSE 1

void yyerror(const char *s); 

%}

%union {
    EvaExpr* Expr;
    std::vector<EvaExpr*>* ListExpr;
}


%token <Expr> TOKEN_NUMBER
%token <Expr> TOKEN_STRING
%token <Expr> TOKEN_SYMBOL

%type <Expr> ROOT_EXPR
%type <Expr> EXPR
%type <Expr> ATOM_EXPR
%type <Expr> LIST_EXPR
%type <ListExpr> LIST_ENTRY_EXPR

%%

ROOT_EXPR
    : EXPR { EvaParser::moveExpr($1); $$ = EvaParser::getRootNodePtr(); }
    | ROOT_EXPR EXPR { EvaParser::moveExpr($2); $$ = EvaParser::getRootNodePtr(); }
    ;

EXPR
    : ATOM_EXPR
    | LIST_EXPR
    ;

ATOM_EXPR
    : TOKEN_NUMBER {  $$ = $1; }
    | TOKEN_STRING {  $$ = $1; }
    | TOKEN_SYMBOL {  $$ = $1; }
    ;


LIST_EXPR
    : '(' LIST_ENTRY_EXPR ')' {  $$ = new EvaExpr(*$2);  delete $2;}
    ;

LIST_ENTRY_EXPR
    : { $$ = new std::vector<EvaExpr*>{}; }
    | LIST_ENTRY_EXPR EXPR { $1->push_back($2); $$ = $1;  }
    ;

%%

void yyerror(const char *s) {
    fprintf(stderr, "Error: %s\n", s);
}

#ifdef DEBUG_MAIN
int main() {
    yyparse();
    if (rootNode != nullptr) {
        rootNode->toString();
        delete rootNode;
    }
    return 0;
}
#endif
