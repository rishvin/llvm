#include "evaLLM.h"

int main(int argc, const char* argv[]) {
    EvaLLM vm{};
    vm.exec(
        R"(
(var VERSION 50)
(print "Value =%d" VERSION) 
)");
    return 0;
}