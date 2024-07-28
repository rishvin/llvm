#pragma once

#include <any>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Value.h>
#include <unordered_map>
#include <utility>

struct EvaValue {
    explicit EvaValue() : value{nullptr} {}
    EvaValue(llvm::Value *value, std::any metadata = {}) :
        value{value}, metadata{std::move(metadata)} {}

    llvm::Value *operator*() const { return value; }

    llvm::Value *value;
    std::any metadata;
};
inline EvaValue EvaValueNull{nullptr};

class EvaEnvironment : public std::enable_shared_from_this<EvaEnvironment> {
public:
    EvaEnvironment(std::unordered_map<std::string, EvaValue> symbols,
                   std::shared_ptr<EvaEnvironment> parent) :
        _symbols{std::move(symbols)}, _parent{parent} {}

    explicit EvaEnvironment(std::shared_ptr<EvaEnvironment> parent) : _parent{parent} {}

    void insert(const std::string &name, const EvaValue &value) { _symbols[name] = value; }

    EvaValue get(const std::string &name) const {
        const auto env = _resolve(name);
        return env != nullptr ? env->_symbols.at(name) : EvaValueNull;
    }

    void setClassScope(llvm::StructType *cls) { clsType = cls; }

    llvm::StructType *getClassScope() const {
        if (clsType != nullptr) {
            return clsType;
        }
        return _parent != nullptr ? _parent->getClassScope() : nullptr;
    }

    void setFunctionScope(llvm::Function *function) { fn = function; }

    llvm::Function *getFunctionScope() const {
        if (fn != nullptr) {
            return fn;
        }

        return _parent != nullptr ? _parent->getFunctionScope() : nullptr;
    }

private:
    std::shared_ptr<EvaEnvironment const> _resolve(const std::string &name) const {
        if (_symbols.contains(name)) {
            return shared_from_this();
        }
        return _parent != nullptr ? _parent->_resolve(name) : nullptr;
    }


    std::unordered_map<std::string, EvaValue> _symbols;
    std::shared_ptr<EvaEnvironment> _parent = nullptr;
    llvm::StructType *clsType = nullptr;
    llvm::Function *fn = nullptr;
};
