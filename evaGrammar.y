%{

#include <iostream>
#include <string>
#include <vector>

#include "evaExp.h"
#include "evaLexer.h"

#define YYDEBUG 1
#define YYERROR_VERBOSE 1

std::vector<EvaExpr*>* rootNode = new std::vector<EvaExpr*>{};

void yyerror(const char *s); 

%}

%union {
    EvaExpr* exp;
    std::vector<EvaExpr*>* listExp;
}


%token <exp> TOKEN_NUMBER
%token <exp> TOKEN_STRING
%token <exp> TOKEN_SYMBOL

%type <listExp> ROOT_EXPR
%type <exp> EXPR
%type <exp> ATOM_EXPR
%type <exp> LIST_EXPR
%type <listExp> LIST_ENTRIES_EXPR

%%

ROOT_EXPR
    : EXPR { rootNode->push_back(std::move($1)); std::cerr << std::endl << "Initialized root node " ; $$ = rootNode;}
    | ROOT_EXPR EXPR { $1->push_back(std::move($2)); std::cerr << std::endl << "Initialized2 root node " ; $$ = rootNode;}
    ;

EXPR
    : ATOM_EXPR
    | LIST_EXPR
    ;

ATOM_EXPR
    : TOKEN_NUMBER { std::cerr << std::endl << "Initaialized number " << $1->toString() << std::endl; $$ = std::move($1); }
    | TOKEN_STRING { std::cerr << std::endl << "Initaialized string " << $1->toString() << std::endl; $$ = std::move($1); }
    | TOKEN_SYMBOL { std::cerr << std::endl << "Initaialized symbol " << $1->toString() << std::endl; $$ = std::move($1); }
    ;


LIST_EXPR
    : '(' LIST_ENTRIES_EXPR ')' {  $$ = new EvaExpr(std::move($2)); std::cerr << std::endl << "Returning the list " << $$->toString() << std::endl; }
    ;

LIST_ENTRIES_EXPR
    : { std::cerr << std::endl <<  "Init LIST_ENTRIES_EXPR" << std::endl; $$ = new std::vector<EvaExpr*>{}; }
    | LIST_ENTRIES_EXPR EXPR { std::cerr << std::endl << "Add ListEntry: " << $2->toString() << std::endl; $1->push_back(std::move($2)); $$ = std::move($1);  }
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
