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

class EvaClassDef : std::enable_shared_from_this<EvaClassDef> {
public:
    static constexpr auto kVTableIndex = 0;

    EvaClassDef(const EvaClassDef&) = delete;

    EvaClassDef& operator=(const EvaClassDef&) = delete;

    [[nodiscard]] std::string getName() const { return _name; }

    std::shared_ptr<const EvaClassDef> getParent() const { return _parent; }

    llvm::StructType* getStruct() const { return _struct; }

    bool hasField(const std::string& fieldName) const {
        return _fields.contains(fieldName) || (_parent && _parent->hasField(fieldName));
    }

    const EvaType& getFieldType(const std::string& fieldName) const {
        if (_fields.contains(fieldName)) {
            return _fields.at(fieldName);
        }

        if (_parent) {
            return _parent->getFieldType(fieldName);
        }

        throw std::runtime_error("Unknown field: " + fieldName);
    }

    llvm::Value* getFieldAddress(llvm::IRBuilder<>& builder, llvm::Value* ptr,
                                 const std::string& fieldName) const {
        if (_fields.contains(fieldName)) {
            const auto fieldIndex = _fields.at(fieldName).metadataAsInt();
            return builder.CreateStructGEP(_struct, ptr, fieldIndex, "f_" + fieldName);
        }

        if (_parent) {
            auto parentAddress =
                    builder.CreateStructGEP(_struct, ptr, kVTableIndex + 1, "add_pptr_" + _name);
            auto parentPtr = builder.CreateLoad(_parent->_struct->getPointerTo(), parentAddress,
                                                "pptr_" + _name);
            return _parent->getFieldAddress(builder, parentPtr, fieldName);
        }

        throw std::runtime_error("Unknown field: " + fieldName);
    }

    llvm::Value* getParentAddress(llvm::IRBuilder<>& builder, llvm::Value* ptr) const {
        if (_parent) {
            return builder.CreateStructGEP(_struct, ptr, kVTableIndex + 1, "add_pptr_" + _name);
        }

        throw std::runtime_error("No parent class");
    }

protected:
    explicit EvaClassDef() = default;

    explicit EvaClassDef(std::string name) : _name{std::move(name)} {}

    virtual ~EvaClassDef() = default;

    EvaClassDef(EvaClassDef&& other) : _name{other._name} {
        _parent = other._parent;

        _struct = other._struct;
        _vTable = other._vTable;

        _fields = std::move(other._fields);
        _methods = std::move(other._methods);
    }

    const std::string _name;

    std::shared_ptr<EvaClassDef> _parent = nullptr;

    llvm::StructType* _struct = nullptr;
    llvm::StructType* _vTable = nullptr;

    std::unordered_map<std::string, EvaType> _fields;
    std::unordered_map<std::string, EvaConstant> _methods;
};

class ImmutableEvaClassDef final : public EvaClassDef {
public:
    explicit ImmutableEvaClassDef(EvaClassDef&& other) : EvaClassDef{std::move(other)} {}
};

class MutableEvaClassDef final : public EvaClassDef {
public:
    MutableEvaClassDef(const std::string& name, llvm::LLVMContext& context,
                       const std::shared_ptr<ImmutableEvaClassDef>& parent = nullptr) :
        EvaClassDef{name} {
        _parent = parent;
        _struct = llvm::StructType::create(context, name);
        _vTable = llvm::StructType::create(context, "vtable_" + name);
    }

    void insertFieldType(const std::string& fieldName, llvm::Type* field) {
        if (hasField(fieldName)) {
            throw std::runtime_error("Field already exists: " + fieldName);
        }
        auto fieldStartIndex = _parent ? kVTableIndex + 2 : kVTableIndex + 1;
        _fields[fieldName] = EvaType{field, static_cast<int>(_fields.size() + fieldStartIndex)};
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

        std::vector<llvm::Type*> structFields{_vTable->getPointerTo()};
        if (_parent) {
            structFields.push_back(_parent->getStruct()->getPointerTo());
        }

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

        return std::make_shared<ImmutableEvaClassDef>(std::move(*this));
    }
};
