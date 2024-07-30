#pragma once

#include "evaEnvironment.h"
#include "evaParser.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

using Env = std::shared_ptr<EvaEnvironment>;

class EvaLLVM {
public:
    struct ClassDef : std::enable_shared_from_this<ClassDef> {
        ClassDef(const std::string& name, llvm::LLVMContext& context, llvm::IRBuilder<>& builder) :
            name{name} {
            _header = llvm::StructType::create(context, {builder.getInt8Ty()->getPointerTo()},
                                               name + "_header"),
            _struct = llvm::StructType::create(context, name);
        }

        llvm::StructType* getStruct() const { return _struct; }

        void setFields(const std::vector<std::pair<std::string, llvm::Type*>>& fields) {
            std::vector<llvm::Type*> structFieldTypes{_header};

            for (int idx = 0; idx < fields.size(); ++idx) {
                auto& [fieldName, fieldType] = fields[idx];
                _fields[fieldName] = EvaType{fieldType, idx + 1};
                structFieldTypes.push_back(fieldType);
            }

            _struct->setBody(structFieldTypes, false);
        }

        const EvaType& getField(const std::string& fieldName) const {
            return _fields.at(fieldName);
        }

        llvm::Value* getFieldAddress(llvm::IRBuilder<>& builder, llvm::Value* ptr,
                                     const std::string& fieldName) const {
            if (!hasField(fieldName)) {
                throw std::runtime_error("Unknown field: " + fieldName + " in class: " + name);
            }

            auto& field = getField(fieldName);
            return builder.CreateStructGEP(_struct, ptr, field.metadataAsInt(), "f_" + fieldName);
        }

        bool hasField(const std::string& fieldName) const { return _fields.contains(fieldName); }

        llvm::StructType* _header = nullptr;
        llvm::StructType* _struct = nullptr;
        std::string name;
        std::unordered_map<std::string, EvaType> _fields;
        std::unordered_map<std::string, llvm::Function*> methods;
    };

    explicit EvaLLVM();

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
