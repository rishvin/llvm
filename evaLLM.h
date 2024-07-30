#pragma once

#include "evaEnvironment.h"
#include "evaParser.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

using Env = std::shared_ptr<EvaEnvironment>;

class EvaLLM {
public:
    struct ClassDef : std::enable_shared_from_this<ClassDef> {
        ClassDef(const std::string& name, llvm::LLVMContext& context) : name{name} {
            underlyingStruct = llvm::StructType::create(context, name);
        }

        llvm::StructType* parent = nullptr;
        llvm::StructType* underlyingStruct;
        std::string name;
        std::unordered_map<std::string, EvaType> fields;
        std::unordered_map<std::string, llvm::Function*> methods;
    };

    explicit EvaLLM();

    void exec(const std::string& program, const std::string& outputFilename) const;

    void testSample() const;

private:
    void _compile(std::unique_ptr<EvaExpr> expr) const;

    EvaType _extractType(const EvaExpr& expr, Env env) const;

    EvaValue _handleOps(const std::unique_ptr<EvaExpr>& expr, Env env) const;

    EvaValue _generate(const std::unique_ptr<EvaExpr>& expr, Env env) const;

    std::shared_ptr<ClassDef> _buildClassDef(const std::unique_ptr<EvaExpr>& expr, Env env) const;

    llvm::Value* _createGlobalVar(const std::string& name, llvm::Constant* init) const;

    llvm::Function* _createFunction(const std::string& name, llvm::FunctionType* fnType,
                                    Env env) const;

    llvm::Function* _createFunctionProto(const std::string& name, llvm::FunctionType* fnType,
                                         Env env) const;

    void _createFunctionBlock(llvm::Function* fn) const;

    llvm::BasicBlock* _createBB(const std::string& name, llvm::Function* fn = nullptr) const;

    llvm::Value* allocateVariable(llvm::Function* fn, const std::string& name, EvaType type,
                                  Env env) const;

    llvm::Value* _getFieldAddress(const EvaValue& clsInstance, std::string& fieldName) const;

    EvaValue _handleFunctionCall(const std::string& fnName, const EvaExpr& argsExpr, Env env) const;


    void _setupExternalFunctions() const;

    void _saveModule(const std::string& filename) const;

    void _setupGlobalEnviroment() const;

    mutable std::unordered_map<std::string, std::shared_ptr<ClassDef>> _classNameToDefMap;
    mutable std::unique_ptr<llvm::LLVMContext> _context;
    mutable std::unique_ptr<llvm::Module> _module;
    mutable std::unique_ptr<llvm::IRBuilder<>> _builder;
    mutable Env _globalEnv = nullptr;
};
