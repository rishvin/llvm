#pragma once

#include "evaEnvironment.h"
#include "evaParser.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

using GlobalEnv = std::shared_ptr<EvaEnvironment>;

class EvaLLM {
public:
    explicit EvaLLM();

    void exec(const std::string& program, const std::string& outputFilename) const;

private:
    void _compile(std::unique_ptr<EvaExpr> expr) const;

    llvm::Value* _generate(const std::unique_ptr<EvaExpr>& expr, GlobalEnv env) const;

    llvm::Value* _createGlobalVar(const std::string& name, llvm::Constant* init) const;

    llvm::Function* _createFunction(const std::string& name, llvm::FunctionType* fnType,
                                    GlobalEnv env) const;

    llvm::Function* _createFunctionProto(const std::string& name, llvm::FunctionType* fnType,
                                         GlobalEnv env) const;

    void _createFunctionBlock(llvm::Function* fn) const;

    llvm::BasicBlock* _createBB(const std::string& name, llvm::Function* fn) const;

    void _setupExternalFunctions() const;

    void _saveModule(const std::string& filename) const;

    void _setupGlobalEnviroment() const;

    mutable std::unique_ptr<llvm::LLVMContext> _context;
    mutable std::unique_ptr<llvm::Module> _module;
    mutable std::unique_ptr<llvm::IRBuilder<>> _builder;
    mutable GlobalEnv _globalEnv = nullptr;
};
