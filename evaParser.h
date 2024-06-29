#pragma once

#include <iostream>
#include <string>
#include <vector>

struct EvaExpr {
    enum class ExpType { Number, String, Symbol, List };

    EvaExpr(const EvaExpr &) = delete;

    EvaExpr &operator=(const EvaExpr &) = delete;

    explicit EvaExpr(int expNumber) : expType{ExpType::Number}, expNumber{expNumber} {
    }

    explicit EvaExpr(std::string expString) {
        if (expString[0] == '"') {
            this->expString = expString.substr(1, expString.size() - 2);
            this->expType = ExpType::String;
        } else {
            this->expString = std::move(expString);
            this->expType = ExpType::Symbol;
        }
    }

    explicit EvaExpr(const std::vector<EvaExpr *> &expList) : expType{ExpType::List} {
        for (auto &exp: expList) {
            this->expList.push_back(std::unique_ptr<EvaExpr>(exp));
        }
    }

    explicit EvaExpr(std::vector<std::unique_ptr<EvaExpr> > expList)
        : expType(ExpType::List), expList(std::move(expList)) {
    }

    [[nodiscard]] std::string toString() const {
        switch (expType) {
            case ExpType::Number:
                return std::move(std::to_string(expNumber));
            case ExpType::String:
            case ExpType::Symbol:
                return expString;
            case ExpType::List: {
                std::string str = "(";
                for (auto &exp: expList) {
                    str += exp->toString() + " ";
                }
                str += ")";
                return std::move(str);
            }
        }
        return "";
    }

    ExpType expType;
    int expNumber{};
    std::string expString;
    std::vector<std::unique_ptr<EvaExpr> > expList;
};

//
// EvaParser
//
class EvaParser {
public:
    std::unique_ptr<EvaExpr> parse(const char *program);

    [[nodiscard]] EvaExpr *getRootNodePtr() const {
        return rootExpr.get();
    }

    void moveExpr(EvaExpr *expr) const {
        rootExpr->expList.push_back(std::unique_ptr<EvaExpr>(expr));
    }

private:
    std::unique_ptr<EvaExpr> rootExpr =
            std::make_unique<EvaExpr>(std::vector<std::unique_ptr<EvaExpr> >{});
};
