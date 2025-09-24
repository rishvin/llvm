
%define api.pure full
%define parse.trace
%define parse.error verbose

%lex-param {void *scanner}
%parse-param {void *scanner} {EvaParser *parser}

%code requires {
#include <evaParser.h>
}

%{

#include <iostream>
#include <string>
#include <vector>
#include <evaGrammar.h>
#include <evaLexer.h>
#include <evaParser.h>

void yyerror(void *locp, EvaParser* parser, const char *s);

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
    : EXPR { parser->moveExpr($1); $$ = parser->getRootNodePtr(); }
    | ROOT_EXPR EXPR { parser->moveExpr($2); $$ = parser->getRootNodePtr(); }
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

void yyerror(void *locp, EvaParser* parser, const char *s) {
    fprintf(stderr, "Error: %s\n", s);
}
