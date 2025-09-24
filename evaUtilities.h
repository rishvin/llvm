#pragma once

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Type.h>
#include <unordered_map>

struct EvaType {
    explicit EvaType() : _type{nullptr} {}

    explicit EvaType(llvm::Type* type) : _type{type} {}

    explicit EvaType(llvm::Type* type, std::string actualType) :
        _type{type}, _actualType{std::move(actualType)} {}

    explicit EvaType(llvm::Type* type, size_t index) : _type{type}, _index{index} {}

    llvm::Type* operator*() const { return _type; }

    [[nodiscard]] size_t index() const {
        if (!_index) {
            throw std::runtime_error("Index not set");
        }
        return *_index;
    }

    [[nodiscard]] const std::string& actualType() const {
        if (!_actualType) {
            throw std::runtime_error("Actual type not set");
        }
        return *_actualType;
    }

private:
    llvm::Type* _type;
    std::optional<std::string> _actualType = std::nullopt;
    std::optional<size_t> _index = std::nullopt;
};

struct EvaValue {
    explicit EvaValue() : _value{nullptr} {}

    explicit EvaValue(llvm::Value* value, std::optional<EvaType> type = std::nullopt) :
        _value{value}, _type{std::move(type)} {}

    [[nodiscard]] llvm::Value* operator*() const { return _value; }

    [[nodiscard]] bool isNull() const { return _value == nullptr; }

    [[nodiscard]] const EvaType& type() const {
        if (!_type) {
            throw std::runtime_error("Type not set");
        }
        return *_type;
    }

private:
    llvm::Value* _value;
    std::optional<EvaType> _type = std::nullopt;
};

static inline EvaValue EvaValueNull{};

struct EvaMethod {
    explicit EvaMethod() : _method{nullptr} {}

    explicit EvaMethod(llvm::Function* method, size_t index) : _method{method}, _index{index} {}

    [[nodiscard]] llvm::Function* operator*() const { return _method; }

    [[nodiscard]] size_t index() const { return _index; }

private:
    llvm::Function* _method;
    size_t _index = 0;
};


class EvaClassDef : std::enable_shared_from_this<EvaClassDef> {
public:
    static constexpr auto kVTableIndex = 0;

    EvaClassDef(const EvaClassDef&) = delete;

    EvaClassDef& operator=(const EvaClassDef&) = delete;

    [[nodiscard]] std::string getName() const { return _name; }

    std::shared_ptr<const EvaClassDef> getParent() const { return _parent; }

    llvm::StructType* getStruct() const { return _struct; }

    bool hasField(const std::string& fieldName) const { return _fields.contains(fieldName); }

    const EvaType& getField(const std::string& fieldName) const {
        if (_fields.contains(fieldName)) {
            return _fields.at(fieldName);
        }

        throw std::runtime_error("Unknown field: " + fieldName + " in class: " + _name);
    }

    llvm::Value* getFieldAddress(llvm::IRBuilder<>& builder, llvm::Value* ptr,
                                 const std::string& fieldName) const {
        if (_fields.contains(fieldName)) {
            const auto fieldIndex = _fields.at(fieldName).index();
            return builder.CreateStructGEP(_struct, ptr, fieldIndex, "f_" + fieldName);
        }

        throw std::runtime_error("Unknown field: " + fieldName + " in class: " + _name);
    }

    EvaValue getMethodInvocation(llvm::IRBuilder<>& builder, llvm::Value* ptr,
                                 const std::string& methodName) const {
        if (!_methods.contains(methodName)) {
            throw std::runtime_error("Unknown method: " + methodName + " in class: " + _name);
        }

        const auto method = _methods.at(methodName);

        auto vTablePtr = builder.CreateStructGEP(_struct, ptr, kVTableIndex, "vtable_ptr");
        auto vTable = builder.CreateLoad(_vTable->getPointerTo(), vTablePtr, "vtable");
        auto methodPtr = builder.CreateStructGEP(_vTable, vTable, method.index(), "method_ptr");
        auto fn = builder.CreateLoad((*method)->getType(), methodPtr, "method");
        return EvaValue{fn, EvaType{(*method)->getFunctionType()}};
    }

    auto& getFields() const { return _fields; }

    bool hasMethod(const std::string& methodName) const { return _methods.contains(methodName); }

    EvaMethod& getMethod(const std::string& methodName) { return _methods.at(methodName); }

    auto& getMethods() const { return _methods; }

    llvm::Constant* getVTableInstance(llvm::Module& module) const {
        return module.getGlobalVariable("i_" + _vTable->getName().str());
    }

    llvm::Value* getVTableAddress(llvm::IRBuilder<>& builder, llvm::Value* ptr) const {
        return builder.CreateStructGEP(_struct, ptr, kVTableIndex, "addr_vtable_ptr");
    }

    std::string toString() const {
        std::stringstream ss;
        ss << "Class: " << _name << "\n";
        ss << "  Fields:\n";

        // commans seperated fields name withing ` `.
        for (const auto& [name, type]: _fields) {
            ss << "`" << name << "`, ";
        }


        ss << "  Methods:\n";
        for (const auto& [name, type]: _methods) {
            ss << "`" << name << "`, ";
        }
        return ss.str();
    }

protected:
    explicit EvaClassDef() = default;

    explicit EvaClassDef(std::string name, std::shared_ptr<EvaClassDef> parent) :
        _name{std::move(name)}, _parent{parent} {}

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
    std::unordered_map<std::string, EvaMethod> _methods;
};

class ImmutableEvaClassDef final : public EvaClassDef {
public:
    explicit ImmutableEvaClassDef(EvaClassDef&& other) : EvaClassDef{std::move(other)} {}
};

class MutableEvaClassDef final : public EvaClassDef {
public:
    MutableEvaClassDef(const std::string& name, llvm::LLVMContext& context,
                       const std::shared_ptr<ImmutableEvaClassDef>& parent = nullptr) :
        EvaClassDef{name, parent} {
        _struct = llvm::StructType::create(context, name);
        _vTable = llvm::StructType::create(context, "vtable_" + name);

        if (_parent) {
            _fields.insert(_parent->getFields().begin(), _parent->getFields().end());
            _methods.insert(_parent->getMethods().begin(), _parent->getMethods().end());
        }
    }

    void insertField(const std::string& fieldName, llvm::Type* field) {
        if (hasField(fieldName)) {
            throw std::runtime_error("Field already exists: " + fieldName);
        }

        constexpr auto fieldStartIndex = kVTableIndex + 1;
        _fields[fieldName] = EvaType{field, _fields.size() + fieldStartIndex};
    }

    void insertMethod(const std::string& methodName, llvm::Function* method) {
        const size_t idx = hasMethod(methodName) ? getMethod(methodName).index() : _methods.size();
        _methods[methodName] = EvaMethod{method, idx};
    }

    std::shared_ptr<ImmutableEvaClassDef> toImmutable(llvm::Module& module) {
        std::vector<llvm::Type*> vTableFields{};
        vTableFields.resize(_methods.size());
        for (auto& [_, method]: _methods) {
            vTableFields[method.index()] = (*method)->getType();
        }
        _vTable->setBody(vTableFields);

        std::vector<llvm::Constant*> vTableValues;
        vTableValues.resize(_methods.size());
        for (auto& [_, method]: _methods) {
            vTableValues[method.index()] = *method;
        }

        module.getOrInsertGlobal("i_" + _vTable->getName().str(), _vTable);
        const auto variable = module.getGlobalVariable("i_" + _vTable->getName().str());

        const auto init = llvm::ConstantStruct::get(_vTable, vTableValues);
        variable->setInitializer(init);
        variable->setAlignment(llvm::MaybeAlign(4));

        std::vector<llvm::Type*> structFields{};
        structFields.resize(_fields.size() + 1);
        structFields[kVTableIndex] = _vTable->getPointerTo();
        for (auto& [_, type]: _fields) {
            structFields[type.index()] = *type;
        }
        _struct->setBody(structFields);

        return std::make_shared<ImmutableEvaClassDef>(std::move(*this));
    }
};
