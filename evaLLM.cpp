#include "evaLLM.h"
#include "evaParser.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"

extern int yydebug;

EvaLLM::EvaLLM() {
    _context = std::make_unique<llvm::LLVMContext>();
    _module = std::make_unique<llvm::Module>("eva", *_context);
    _builder = std::make_unique<llvm::IRBuilder<>>(*_context);

    yydebug = 1;

    _setupExternalFunctions();
}

void EvaLLM::exec(const std::string& program, const std::string& outputFilename) const {
    const auto parser = std::make_unique<EvaParser>();

    _compile(std::move(parser->parse(program.c_str())));
    _saveModule(outputFilename);
}

void EvaLLM::_compile(std::unique_ptr<EvaExpr> expr) const {
    const llvm::Function* mainFn =
            _createFunction("main", llvm::FunctionType::get(_builder->getInt32Ty(), false));

    auto result = _generate(expr);
    result = _builder->CreateIntCast(result, _builder->getInt32Ty(), true);
    _builder->CreateRet(result);

    verifyFunction(*mainFn);
}

llvm::Value* EvaLLM::_generate(const std::unique_ptr<EvaExpr>& expr) const {
    switch (expr->expType) {
        case EvaExpr::ExpType::Number:
            return _builder->getInt32(expr->expNumber);

        case EvaExpr::ExpType::String:
            return _builder->CreateGlobalStringPtr(expr->expString);

        case EvaExpr::ExpType::Symbol:
            if (expr->expString == "VERSION") {
                auto var = _module->getGlobalVariable(expr->expString);
                return var->getInitializer();
            }
            return _builder->getInt32(0);

        case EvaExpr::ExpType::List: {
            if (const auto& tag = expr->expList.front(); tag->expType == EvaExpr::ExpType::Symbol) {
                const auto& op = tag->expString;
                if (op == "print") {
                    const auto& printFn = _module->getFunction("printf");
                    std::vector<llvm::Value*> args{};
                    for (auto i = 1; i < expr->expList.size(); i++) {
                        auto& subExpr = expr->expList.at(i);
                        auto arg = _generate(subExpr);
                        args.push_back(arg);
                    }
                    return _builder->CreateCall(printFn, args);
                }
                if (op == "var") {
                    const auto& subExpr = expr->expList.at(2);
                    const auto& name = expr->expList.at(1)->expString;
                    const auto& init = _generate(subExpr);

                    _createGlobalVar(name, llvm::dyn_cast<llvm::Constant>(init));
                    return init;
                }

                throw std::runtime_error("Unknown operator: " + op);
            }

            for (const auto& subExpr: expr->expList) {
                std::ignore = _generate(subExpr);
            }
            return _builder->getInt32(0);
        }
    }

    return _builder->getInt32(0);
}

llvm::Value* EvaLLM::_createGlobalVar(const std::string& name, llvm::Constant* init) const {
    _module->getOrInsertGlobal(name, init->getType());
    auto variable = _module->getGlobalVariable(name);
    variable->setInitializer(init);
    variable->setAlignment(llvm::MaybeAlign(4));
    return variable;
}

llvm::Function* EvaLLM::_createFunction(const std::string& name, llvm::FunctionType* fnType) const {
    auto fn = _module->getFunction(name);
    if (!fn) {
        fn = _createFunctionProto(name, fnType);
    }

    _createFunctionBlock(fn);
    return fn;
}

llvm::Function* EvaLLM::_createFunctionProto(const std::string& name,
                                             llvm::FunctionType* fnType) const {
    return llvm::Function::Create(fnType, llvm::Function::ExternalLinkage, name, *_module);
}

void EvaLLM::_createFunctionBlock(llvm::Function* fn) const {
    auto entry = _createBB("entry", fn);
    _builder->SetInsertPoint(entry);
}

llvm::BasicBlock* EvaLLM::_createBB(const std::string& name, llvm::Function* fn) const {
    return llvm::BasicBlock::Create(*_context, name, fn);
}

void EvaLLM::_setupExternalFunctions() const {
    _module->getOrInsertFunction(
            "printf", llvm::FunctionType::get(_builder->getInt32Ty(),
                                              _builder->getInt8Ty()->getPointerTo(), true));
}

void EvaLLM::_saveModule(const std::string& filename) const {
    std::error_code error;
    llvm::raw_fd_ostream file(filename, error);
    _module->print(file, nullptr);
}

int main(int argc, const char* argv[]) {
    const std::string program =
            R"(
(var VERSION 50)
(print "Value =%d" VERSION)
)";

    EvaLLM vm{};
    vm.exec(program, "eva.ll");
    return 0;
}
