#pragma once

#include <llvm/IR/Value.h>
#include <unordered_map>

class EvaEnvironment : public std::enable_shared_from_this<EvaEnvironment> {
public:
    EvaEnvironment(std::unordered_map<std::string, llvm::Value *> symbols,
                   std::shared_ptr<EvaEnvironment> parent) :
        _symbols{std::move(symbols)}, _parent{std::move(parent)} {}
    void insert(const std::string &name, llvm::Value *value) { _symbols[name] = value; }

    llvm::Value *get(const std::string &name) const {
        const auto env = _resolve(name);
        return env != nullptr ? env->_symbols.at(name) : nullptr;
    }

private:
    std::shared_ptr<EvaEnvironment const> _resolve(const std::string &name) const {
        if (_symbols.contains(name)) {
            return shared_from_this();
        }
        return _parent != nullptr ? _parent->_resolve(name) : nullptr;
    }

    std::unordered_map<std::string, llvm::Value *> _symbols;
    std::shared_ptr<EvaEnvironment> _parent = nullptr;
};
