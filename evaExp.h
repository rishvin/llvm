#pragma once

#include <iostream>
#include <string>
#include <vector>

//
// EvaExpr
//
struct EvaExpr {
    enum class ExpType { Number, String, Symbol, List };

    ExpType                expType;
    int                    expNumber;
    std::string            expString;
    std::vector<EvaExpr*>* expList;

    EvaExpr(int number) : expType{ExpType::Number}, expNumber{number} {
    }

    EvaExpr(const std::string strVal) {
        if (strVal[0] == '"') {
            expType   = ExpType::String;
            expString = strVal.substr(1, strVal.size() - 2);
        } else {
            expType   = ExpType::Symbol;
            expString = std::move(strVal);
        }
    }

    EvaExpr(std::vector<EvaExpr*>* expList) : expType(ExpType::List), expList(std::move(expList)) {
    }

    std::string toString() {
        switch (expType) {
            case ExpType::Number:
                return std::to_string(expNumber);
            case ExpType::String:
                return expString;
            case ExpType::Symbol:
                return expString;
            case ExpType::List: {
                std::string str = "(";
                for (auto& exp : *expList) {
                    str += exp->toString() + " ";
                }
                str += ")";
                return str;
            }
        }
    }
};
