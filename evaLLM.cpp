#include "evaLLM.h"
#include "evaParser.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"

extern int yydebug;

#define BINARY_OP(OP, NAME)                             \
    auto lhs = _generate(expr->expList.at(1), env, fn); \
    auto rhs = _generate(expr->expList.at(2), env, fn); \
    return _builder->OP(lhs, rhs, NAME);

EvaLLM::EvaLLM() {
    _context = std::make_unique<llvm::LLVMContext>();
    _module = std::make_unique<llvm::Module>("eva", *_context);
    _builder = std::make_unique<llvm::IRBuilder<>>(*_context);

    yydebug = 1;

    _setupExternalFunctions();
    _setupGlobalEnviroment();
}

void EvaLLM::exec(const std::string& program, const std::string& outputFilename) const {
    const auto parser = std::make_unique<EvaParser>();

    _compile(std::move(parser->parse(program.c_str())));
    _saveModule(outputFilename);
}

void EvaLLM::_compile(std::unique_ptr<EvaExpr> expr) const {
    llvm::Function* mainFn = _createFunction(
            "main", llvm::FunctionType::get(_builder->getInt32Ty(), false), _globalEnv);

    auto result = _generate(expr, _globalEnv, mainFn);
    result = _builder->CreateIntCast(result, _builder->getInt32Ty(), true);
    _builder->CreateRet(result);

    verifyFunction(*mainFn);
}

llvm::Type* EvaLLM::toType(const std::string& type) const {
    if (type == "number") {
        return _builder->getInt32Ty();
    }
    if (type == "string") {
        return _builder->getInt8Ty()->getPointerTo();
    }
    return _builder->getInt32Ty();
}

llvm::Value* EvaLLM::handleOps(const std::unique_ptr<EvaExpr>& expr, Env env,
                               llvm::Function* fn) const {
    auto op = expr->expList.front()->expString;

    if (op == "print") {
        const auto& printFn = _module->getFunction("printf");
        std::vector<llvm::Value*> args{};
        for (auto i = 1; i < expr->expList.size(); i++) {
            auto& subExpr = expr->expList.at(i);
            auto arg = _generate(subExpr, env, fn);
            args.push_back(arg);
        }
        return _builder->CreateCall(printFn, args);
    }

    if (op == "var") {
        // var x 10
        // var x VARIABLE
        // var (x number) 10
        // var (x number) VARIABLE
        // var (x number) (+ y 10)

        auto extractName = [](const auto& subExpr) -> std::string {
            if (subExpr->expType == EvaExpr::ExpType::List) {
                return subExpr->expList.at(0)->expString;
            }
            return subExpr->expString;
        };

        auto extractType = [](const auto& subExpr) -> std::string {
            if (subExpr->expType == EvaExpr::ExpType::List) {
                return subExpr->expList.at(1)->expString;
            }
            return "number";
        };

        const auto& name = extractName(expr->expList.at(1));
        const auto& type = toType(extractType(expr->expList.at(1)));

        const auto& subExpr = expr->expList.at(2);
        const auto& init = _generate(subExpr, env, fn);

        auto varBinding = allocateVariable(fn, name, type, env);
        return _builder->CreateStore(init, varBinding);

        //_createGlobalVar(name, llvm::dyn_cast<llvm::Constant>(init));
        // return init;
    }

    if (op == "begin") {
        auto currentEnv = std::make_shared<EvaEnvironment>(env);
        llvm::Value* result = nullptr;
        for (auto i = 1; i < expr->expList.size(); i++) {
            auto& subExpr = expr->expList.at(i);
            result = _generate(subExpr, currentEnv, fn);
        }
        return result;
    }

    if (op == "set") {
        // set x 10
        auto name = expr->expList.at(1)->expString;
        auto valBinding = env->get(name);
        auto newValue = _generate(expr->expList.at(2), env, fn);
        auto result = _builder->CreateStore(newValue, valBinding);
        return result;
    }

    // (if (== x 10) (print "x is 10") (print "x is not 10"))
    if (op == "if") {
        // auto condition = _generate(expr->expList.at(1), env, fn);
        // auto thenBB = _createBB("then", fn);
        // auto elseBB = _createBB("else", fn);
    }

    if (op == "+") {
        BINARY_OP(CreateAdd, "add");
    }
    if (op == "-") {
        BINARY_OP(CreateSub, "sub");
    }
    if (op == "*") {
        BINARY_OP(CreateMul, "mul");
    }
    if (op == "/") {
        BINARY_OP(CreateSDiv, "div");
    }

    throw std::runtime_error("Unknown operator: " + op);
}

llvm::Value* EvaLLM::_generate(const std::unique_ptr<EvaExpr>& expr, Env env,
                               llvm::Function* fn) const {
    switch (expr->expType) {
        case EvaExpr::ExpType::Number:
            return _builder->getInt32(expr->expNumber);

        case EvaExpr::ExpType::String:
            return _builder->CreateGlobalStringPtr(expr->expString);

        case EvaExpr::ExpType::Symbol: {
            const auto symbol = env->get(expr->expString);

            if (const auto localVar = llvm::dyn_cast<llvm::AllocaInst>(symbol)) {
                return _builder->CreateLoad(localVar->getAllocatedType(), localVar,
                                            expr->expString);
            }

            if (const auto globalVar = llvm::dyn_cast<llvm::GlobalVariable>(symbol)) {
                return _builder->CreateLoad(globalVar->getInitializer()->getType(), globalVar,
                                            symbol->getName());
            }
            return _builder->getInt32(0);
        }
        case EvaExpr::ExpType::List: {
            if (const auto& subExpr = expr->expList.front();
                subExpr->expType == EvaExpr::ExpType::Symbol) {
                return handleOps(expr, env, fn);
            }

            for (const auto& subExpr: expr->expList) {
                std::ignore = _generate(subExpr, env, fn);
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

llvm::Function* EvaLLM::_createFunction(const std::string& name, llvm::FunctionType* fnType,
                                        Env env) const {
    auto fn = _module->getFunction(name);
    if (!fn) {
        fn = _createFunctionProto(name, fnType, env);
    }

    _createFunctionBlock(fn);
    return fn;
}

llvm::Function* EvaLLM::_createFunctionProto(const std::string& name, llvm::FunctionType* fnType,
                                             Env env) const {
    auto fn = llvm::Function::Create(fnType, llvm::Function::ExternalLinkage, name, *_module);
    env->insert(name, fn);
    return fn;
}

void EvaLLM::_createFunctionBlock(llvm::Function* fn) const {
    auto entry = _createBB("entry", fn);
    _builder->SetInsertPoint(entry);
}

llvm::BasicBlock* EvaLLM::_createBB(const std::string& name, llvm::Function* fn) const {
    return llvm::BasicBlock::Create(*_context, name, fn);
}

llvm::Value* EvaLLM::allocateVariable(llvm::Function* fn, const std::string& name, llvm::Type* type,
                                      const Env env) const {
    const auto currentBuilder = std::make_unique<llvm::IRBuilder<>>(*_context);
    currentBuilder->SetInsertPoint(&fn->getEntryBlock(), fn->getEntryBlock().begin());
    const auto allocVar = currentBuilder->CreateAlloca(type, nullptr, name);
    env->insert(name, allocVar);
    return allocVar;
}

void EvaLLM::_setupExternalFunctions() const {
    _module->getOrInsertFunction(
            "printf", llvm::FunctionType::get(_builder->getInt32Ty(),
                                              _builder->getInt8Ty()->getPointerTo(), true));
}

void EvaLLM::_setupGlobalEnviroment() const {
    const std::unordered_map<std::string, llvm::Value*> globals{
            {"VERSION", _builder->getInt32(1001)}};

    std::unordered_map<std::string, llvm::Value*> globalVars{};
    for (auto& [name, value]: globals) {
        globalVars[name] = _createGlobalVar(name, llvm::dyn_cast<llvm::Constant>(value));
    }
    _globalEnv = std::make_shared<EvaEnvironment>(globalVars, nullptr);
}


void EvaLLM::_saveModule(const std::string& filename) const {
    std::error_code error;
    llvm::raw_fd_ostream file(filename, error);
    _module->print(file, nullptr);
}

int main(int argc, const char* argv[]) {
    const std::string program =
            R"(
 (begin
    (var VERSION 1001)

    (begin
       (var VERSION "chandan")
       (print "Value =%s" VERSION)
    )

    (var x 10)
    (var y (+ x 50))
    (print "Value =%d\n" y)
    (print "Value =%d" VERSION)
 )

 )";

    EvaLLM vm{};
    vm.exec(program, "eva.ll");
    return 0;
}
