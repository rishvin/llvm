#pragma once

#include "evaEnvironment.h"
#include "evaParser.h"
#include "evaUtilities.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

using Env = std::shared_ptr<EvaEnvironment>;

class EvaLLVM {
public:

    explicit EvaLLVM();

    void exec(const std::string& program, const std::string& outputFilename) const;

    void testSample() const;

private:
    void _compile(std::unique_ptr<EvaExpr> expr) const;

    EvaType _extractType(const EvaExpr& expr, Env env) const;

    EvaValue _handleOps(const std::unique_ptr<EvaExpr>& expr, Env env) const;

    EvaValue _generate(const std::unique_ptr<EvaExpr>& expr, Env env) const;

    std::shared_ptr<MutableEvaClassDef> _buildClassDef(const std::unique_ptr<EvaExpr>& expr,
                                                       Env env) const;

    llvm::Value* _createGlobalVar(const std::string& name, llvm::Constant* init) const;

    llvm::Function* _createFunction(const std::string& name, llvm::FunctionType* fnType,
                                    Env env) const;

    llvm::Function* _createFunctionProto(const std::string& name, llvm::FunctionType* fnType,
                                         Env env) const;

    void _createFunctionBlock(llvm::Function* fn) const;

    llvm::BasicBlock* _createBB(const std::string& name, llvm::Function* fn = nullptr) const;

    llvm::Value* _allocateStackVariable(llvm::Function* fn, const std::string& name, EvaType type,
                                        Env env) const;

    EvaValue _allocateHeapVariable(const std::string& name, EvaType type, Env env) const;

    llvm::Value* _getFieldAddress(const EvaValue& clsInstance, std::string& fieldName,
                                  Env env) const;

    EvaClassDef* _resolveClass(const std::string& className, Env env) const;

    EvaValue _handleFunctionCall(const std::string& fnName, const EvaExpr& argsExpr, Env env) const;

    void _initClass(const EvaValue& clsInstance, const EvaClassDef& clsDef, const EvaExpr& initArgs,
                    Env env) const;


    void _setupExternalFunctions() const;

    void _saveModule(const std::string& filename) const;

    void _setupGlobalEnviroment() const;

    mutable std::unordered_map<std::string, std::shared_ptr<ImmutableEvaClassDef>>
            _classNameToDefMap;
    mutable std::unique_ptr<llvm::LLVMContext> _context;
    mutable std::unique_ptr<llvm::Module> _module;
    mutable std::unique_ptr<llvm::IRBuilder<>> _builder;
    mutable Env _globalEnv = nullptr;
};
