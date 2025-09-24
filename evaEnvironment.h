#pragma once

#include <any>
#include <unordered_map>

#include "evaUtilities.h"

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

    void setClassScope(MutableEvaClassDef *cls) { _scopedClass = cls; }

    MutableEvaClassDef *getClassScope() const {
        if (_scopedClass != nullptr) {
            return _scopedClass;
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
    MutableEvaClassDef *_scopedClass = nullptr;
    llvm::Function *fn = nullptr;
};
