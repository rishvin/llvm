#pragma once

#include "evaParser.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"

extern int yydebug;

class EvaLLM {
public:
    EvaLLM() {
        moduleInit();
        setupExternalFunctions();
    }

    void exec(const char* program) {
        yydebug     = 1;
        auto parser = std::make_unique<EvaParser>();
        std::cout << "Parsing program: " << program << std::endl;
        auto expr = parser->parse(program);
        compile(std::move(expr));
        saveModuleFromFile("eva.ll");
    }

private:
    void moduleInit() {
        context = std::make_unique<llvm::LLVMContext>();
        module  = std::make_unique<llvm::Module>("eva", *context);
        builder = std::make_unique<llvm::IRBuilder<>>(*context);
    }

    void compile(std::unique_ptr<EvaExpr> expr) {
        fn          = createFunction("main", llvm::FunctionType::get(builder->getInt32Ty(), false));
        auto result = gen(std::move(expr));

        result = builder->CreateIntCast(result, builder->getInt32Ty(), true);
        builder->CreateRet(result);
        verifyFunction(*fn);
    }

    llvm::Value* gen(std::unique_ptr<EvaExpr> expr) {
        switch (expr->expType) {
            case EvaExpr::ExpType::Number:
                return builder->getInt32(expr->expNumber);
            case EvaExpr::ExpType::String:
                return builder->CreateGlobalStringPtr(expr->expString.c_str());
            case EvaExpr::ExpType::Symbol:
                if (expr->expString == "VERSION") {
                    auto var = module->getGlobalVariable(expr->expString);
                    return var->getInitializer();
                }
                return builder->getInt32(0);
            case EvaExpr::ExpType::List: {
                auto& tag = expr->expList.front();
                if (tag->expType == EvaExpr::ExpType::Symbol) {
                    auto op = tag->expString;
                    if (op == "print") {
                        auto                      printFn = module->getFunction("printf");
                        std::vector<llvm::Value*> args{};
                        for (auto i = 1; i < expr->expList.size(); i++) {
                            auto& subExpr = expr->expList.at(i);
                            auto  arg     = gen(std::move(subExpr));
                            args.push_back(arg);
                        }
                        return builder->CreateCall(printFn, args);
                    } else if (op == "var") {
                        auto& subExpr = expr->expList.at(2);
                        auto  name    = expr->expList.at(1)->expString;
                        auto  init    = gen(std::move(subExpr));
                        createGlobalVar(name, static_cast<llvm::Constant*>(init));
                        return init;
                    }
                } else {
                    for (auto& subExpr : expr->expList) {
                        gen(std::move(subExpr));
                    }
                    return builder->getInt32(0);
                }
                return builder->getInt32(0);
            }
        }
        return builder->getInt32(0);
    }

    llvm::Value* createGlobalVar(const std::string& name, llvm::Constant* init) {
        module->getOrInsertGlobal(name, init->getType());
        auto variable = module->getGlobalVariable(name);
        variable->setInitializer(init);
        variable->setAlignment(llvm::MaybeAlign(4));
        return variable;
    }

    llvm::Function* createFunction(const std::string& name, llvm::FunctionType* fnType) {
        auto fn = module->getFunction(name);
        if (!fn) {
            fn = createFunctionProto(name, fnType);
        }

        createFunctionBlock(fn);
        return fn;
    }

    llvm::Function* createFunctionProto(const std::string& name, llvm::FunctionType* fnType) {
        auto fn = llvm::Function::Create(fnType, llvm::Function::ExternalLinkage, name, *module);
        return fn;
    }

    void createFunctionBlock(llvm::Function* fn) {
        auto entry = createBB("entry", fn);
        builder->SetInsertPoint(entry);
    }

    llvm::BasicBlock* createBB(const std::string& name, llvm::Function* fn) {
        return llvm::BasicBlock::Create(*context, name, fn);
    }

    void setupExternalFunctions() {
        module->getOrInsertFunction("printf",
                                    llvm::FunctionType::get(builder->getInt32Ty(),
                                                            builder->getInt8Ty()->getPointerTo(),
                                                            true));
    }

    void saveModuleFromFile(const std::string& filename) {
        std::error_code      error;
        llvm::raw_fd_ostream file(filename, error);
        module->print(file, nullptr);
    }

    llvm::Function* fn;

    std::unique_ptr<llvm::LLVMContext> context;
    std::unique_ptr<llvm::Module>      module;
    std::unique_ptr<llvm::IRBuilder<>> builder;
};
