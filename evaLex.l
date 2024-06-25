%{
#include <iostream>
#include <string>

#include "evaExp.h"
#include "evaGrammar.h"

#define YY_FLEX_DEBUG 1
%}

%option header-file="evaLexer.h"
%option noyywrap     


%%

\s+                /* ignore whitespace */
\/\/.*             /* ignore comments */
\/\*[\s\S]*?\*\/   /* ignore comments */
-?[0-9]+           { yylval.exp = new EvaExpr(atoi(yytext)); std::cerr << std::endl << "Num token: " << yytext << std::endl ;return TOKEN_NUMBER; }
\"[^\"]*\"         { yylval.exp = new EvaExpr(yytext); std::cerr << std::endl << "String token: " << yytext << std::endl; return TOKEN_STRING; }
[a-zA-Z0-9_\-+*=!<>/]+     { yylval.exp = new EvaExpr(yytext); std::cerr << std::endl << "Symbol token: " << yytext << std::endl; return TOKEN_SYMBOL; }
\(                 { std::cerr << std::endl << "Left Paren token: (" << std::endl; return '('; }
\)                 { std::cerr << std::endl << "Right Paren token: )" << std::endl; return ')'; }


%%


void showError(){
    printf("Other input");
}
