#pragma once

#include <iostream>
#include <string>
#include <vector>

#include "evaExp.h"
#include "evaGrammar.h"
#include "evaLexer.h"

extern int yyparse();

//
// EvaParser
//
extern std::vector<EvaExpr*>* rootNode;

class EvaParser {
public:
    EvaParser() = default;

    ~EvaParser() = default;

    std::unique_ptr<std::vector<EvaExpr*>> parse(const char* program) {
        YY_BUFFER_STATE buffer = yy_scan_string(program);
        yy_switch_to_buffer(buffer);

        // Parse the input
        if (yyparse() == 0) {
            yy_delete_buffer(buffer);
            auto exps = std::make_unique<std::vector<EvaExpr*>>(*rootNode);
            return exps;
        } else {
            yy_delete_buffer(buffer);
            return nullptr;
        }
    }
};