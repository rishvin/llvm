#include "evaParser.h"
#include <evaGrammar.h>
#include <evaLexer.h>

std::unique_ptr<EvaExpr> EvaParser::parse(const char* program) {
    yyscan_t scanner;

    // Initialize the scanner
    yylex_init(&scanner);
    yyset_extra(this, scanner);

    // Scan the input string
    YY_BUFFER_STATE buffer = yy_scan_string(program, scanner);
    yy_switch_to_buffer(buffer, scanner);

    // Parse the input
    yyparse(scanner, this);

    // Cleanup
    yy_delete_buffer(buffer, scanner);
    yylex_destroy(scanner);

    std::cout << "Parsed: " << rootExpr->toString() << std::endl;
    return std::move(rootExpr);
}
