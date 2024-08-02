#pragma once

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Type.h>
#include <unordered_map>
#include <utility>

template<typename VType>
struct AbstractEvaValue {
    explicit AbstractEvaValue(VType value, std::any metadata) :
        value{value}, metadata{std::move(metadata)} {}

    virtual ~AbstractEvaValue() = default;

    [[nodiscard]] virtual bool isNull() const = 0;

    virtual VType operator*() const { return value; }

    VType value;
    std::any metadata;
};

struct EvaValue final : AbstractEvaValue<llvm::Value*> {
    explicit EvaValue() : AbstractEvaValue{nullptr, {}} {}
    EvaValue(llvm::Value* value, std::any metadata = {}) :
        AbstractEvaValue{value, std::move(metadata)} {}

    [[nodiscard]] bool isNull() const override { return value == nullptr; }
};

inline EvaValue EvaValueNull{nullptr};

struct EvaConstant final : AbstractEvaValue<llvm::Constant*> {
    explicit EvaConstant() : AbstractEvaValue{nullptr, {}} {}
    EvaConstant(llvm::Constant* value, std::any metadata) :
        AbstractEvaValue{value, std::move(metadata)} {}

    [[nodiscard]] bool isNull() const override { return value == nullptr; }
};

struct EvaType {
    explicit EvaType() : type{nullptr} {}
    explicit EvaType(llvm::Type* type, std::any metadata = {}) :
        type{type}, metadata{std::move(metadata)} {}

    llvm::Type* operator*() const { return type; }

    [[nodiscard]] int metadataAsInt() const { return std::any_cast<int>(metadata); }

    [[nodiscard]] std::string metadataAsString() const {
        return std::any_cast<std::string>(metadata);
    }

    llvm::Type* type;
    std::any metadata;
};

class ImmutableEvaClassDef : std::enable_shared_from_this<ImmutableEvaClassDef> {
public:
    ImmutableEvaClassDef() = default;

    ImmutableEvaClassDef(std::string& name, llvm::StructType* structType,
                         llvm::StructType* vTableType,
                         std::unordered_map<std::string, EvaType> fields,
                         std::unordered_map<std::string, EvaConstant> methods) :
        _name{std::move(name)}, _struct{structType}, _vTable{vTableType},
        _fields{std::move(fields)}, _methods{std::move(methods)} {}

    [[nodiscard]] std::string getName() const { return _name; }

    llvm::StructType* getStruct() const { return _struct; }

    const EvaType& getField(const std::string& fieldName) const { return _fields.at(fieldName); }

    bool hasField(const std::string& fieldName) const { return _fields.contains(fieldName); }

    llvm::Value* getFieldAddress(llvm::IRBuilder<>& builder, llvm::Value* ptr,
                                 const std::string& fieldName) const {
        if (!hasField(fieldName)) {
            throw std::runtime_error("Unknown field: " + fieldName + " in class: " + _name);
        }

        auto& field = getField(fieldName);
        return builder.CreateStructGEP(_struct, ptr, field.metadataAsInt(), "f_" + fieldName);
    }

protected:
    std::string _name;

    llvm::StructType* _struct = nullptr;
    llvm::StructType* _vTable = nullptr;

    std::unordered_map<std::string, EvaType> _fields;
    std::unordered_map<std::string, EvaConstant> _methods;
};

class MutableEvaClassDef : public ImmutableEvaClassDef {
public:
    MutableEvaClassDef(const std::string& name, llvm::LLVMContext& context) :
        ImmutableEvaClassDef{} {
        _name = name;
        _struct = llvm::StructType::create(context, name);
        _vTable = llvm::StructType::create(context, "vtable_" + name);
    }

    void setField(const std::string& fieldName, llvm::Type* field) {
        constexpr int fieldStartIdx = 1;
        int idx = _fields.size() + fieldStartIdx;
        _fields[fieldName] = EvaType{field, idx};
    }

    void setMethod(const std::string& methodName, llvm::Function* method) {
        int idx = _methods.size();
        _methods[methodName] = EvaConstant{method, idx};
    }

    std::shared_ptr<ImmutableEvaClassDef> toImmutable(llvm::Module& module) {
        std::vector<llvm::Type*> vTableFields{};
        for (auto& [_, method]: _methods) {
            vTableFields.push_back((*method)->getType());
        }
        _vTable->setBody(vTableFields);

        std::vector<llvm::Type*> structFields{_vTable};
        for (auto& [_, type]: _fields) {
            structFields.push_back(*type);
        }
        _struct->setBody(structFields);

        module.getOrInsertGlobal("i_" + _vTable->getName().str(), _vTable);
        auto variable = module.getGlobalVariable("i_" + _vTable->getName().str());

        std::vector<llvm::Constant*> vTableValues;
        for (auto& [_, method]: _methods) {
            vTableValues.push_back(*method);
        }

        auto init = llvm::ConstantStruct::get(_vTable, vTableValues);
        variable->setInitializer(init);
        variable->setAlignment(llvm::MaybeAlign(4));

        return std::make_shared<ImmutableEvaClassDef>(_name, _struct, _vTable, _fields, _methods);
    }
};
