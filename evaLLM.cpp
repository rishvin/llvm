#include <fstream>
#include <sstream>

#include "evaLLM.h"
#include "evaParser.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"

extern int yydebug;

#define BINARY_OP(OP, NAME)                         \
    auto lhs = _generate(expr->expList.at(1), env); \
    auto rhs = _generate(expr->expList.at(2), env); \
    return EvaValue{_builder->OP(*lhs, *rhs, NAME)};

namespace {

    auto extractName = [](const auto& subExpr) -> std::string {
        if (subExpr->expType == EvaExpr::ExpType::List) {
            return subExpr->expList.at(0)->expString;
        }
        return subExpr->expString;
    };

    std::string unRawString(const std::string& raw) {
        std::string output;
        output.reserve(raw.size());
        for (auto i = 0; i < raw.size(); i++) {
            if (raw[i] == '\\' && i + 1 < raw.size()) {
                switch (raw[i + 1]) {
                    case 'n':
                        output += '\n';
                        i += 1;
                        break;
                    case 't':
                        output += '\t';
                        i += 1;
                        break;
                    case 'r':
                        output += '\r';
                        i += 1;
                        break;
                    case 'f':
                        output += '\f';
                        i += 1;
                        break;
                    case 'v':
                        output += '\v';
                        i += 1;
                        break;
                    case 'a':
                        output += '\a';
                        i += 1;
                        break;
                    case 'b':
                        output += '\b';
                        i += 1;
                        break;
                    case '\\':
                        output += '\\';
                        i += 1;
                        break;
                    default:
                        output += raw[i];
                }
            } else {
                output += raw[i];
            }
        }

        return output;
    }
} // namespace


EvaLLVM::EvaLLVM() {
    _context = std::make_unique<llvm::LLVMContext>();
    _module = std::make_unique<llvm::Module>("eva", *_context);
    _builder = std::make_unique<llvm::IRBuilder<>>(*_context);

    yydebug = 1;

    _setupExternalFunctions();
    _setupGlobalEnviroment();
}

void EvaLLVM::exec(const std::string& program, const std::string& outputFilename) const {
    const auto parser = std::make_unique<EvaParser>();

    _compile(std::move(parser->parse(program.c_str())));
    _saveModule(outputFilename);
}

void EvaLLVM::_compile(std::unique_ptr<EvaExpr> expr) const {
    llvm::Function* mainFn = _createFunction(
            "main", llvm::FunctionType::get(_builder->getInt32Ty(), false), _globalEnv);

    // Set the current scope of the function to `main` such that all the code generatio will happen
    // inside this.
    _globalEnv->setFunctionScope(mainFn);

    auto genValue = _generate(expr, _globalEnv);
    auto mainRetValue = _builder->CreateIntCast(*genValue, _builder->getInt32Ty(), true);
    _builder->CreateRet(mainRetValue);

    verifyFunction(*mainFn);
}

EvaType EvaLLVM::_extractType(const EvaExpr& expr, Env env) const {
    auto toType = [&](const std::string& strType) {
        // The `self` keyword is used to refer to the current class instance.
        if (strType == "self") {
            auto scopedCls = env->getClassScope();
            if (!scopedCls) {
                throw std::runtime_error("self is not allowed here");
            }
            return EvaType{scopedCls->getStruct()->getPointerTo(),
                           scopedCls->getStruct()->getName().str()};
        }

        if (strType == "number") {
            return EvaType{_builder->getInt32Ty(), "int32"};
        }

        if (strType == "string") {
            return EvaType{_builder->getInt8Ty()->getPointerTo(), "str"};
        }

        // Case when the type is a class name.
        if (auto clsDef = _resolveClass(strType, env)) {
            return EvaType{clsDef->getStruct()->getPointerTo(), clsDef->getName()};
        }

        return EvaType{_builder->getInt32Ty(), "int32"};
    };

    if (expr.expType == EvaExpr::ExpType::List) {
        if (expr.expList.at(0)->expString == "self") {
            return toType("self");
        }

        return toType(expr.expList.at(1)->expString);
    }
    return toType(expr.expString);
}

EvaValue EvaLLVM::_handleOps(const std::unique_ptr<EvaExpr>& expr, Env env) const {
    auto op = expr->expList.front()->expString;

    if (op == "print") {
        const auto& printFn = _module->getFunction("printf");
        std::vector<llvm::Value*> args{};

        for (auto i = 1; i < expr->expList.size(); i++) {
            auto& subExpr = expr->expList.at(i);
            auto arg = _generate(subExpr, env);
            args.push_back(*arg);
        }

        return EvaValue{_builder->CreateCall(printFn, args)};
    }

    if (op == "var") {
        // var x 10
        // var x VARIABLE
        // var (x number) 10
        // var (x number) VARIABLE
        // var (x number) (+ y 10)

        const auto& name = extractName(expr->expList.at(1));

        // This cls parameter is used to handle the self keyword when someone creates a variable
        // inside the struct body.
        const auto type = _extractType(*expr->expList.at(1), env);

        const auto& subExpr = expr->expList.at(2);
        const auto& init = _generate(subExpr, env);

        auto varBinding = _allocateStackVariable(env->getFunctionScope(), name, type, env);
        _builder->CreateStore(*init, varBinding);

        return init;
    }

    if (op == "begin") {
        auto currentEnv = std::make_shared<EvaEnvironment>(env);
        EvaValue result = EvaValueNull;
        for (auto i = 1; i < expr->expList.size(); i++) {
            auto& subExpr = expr->expList.at(i);
            result = _generate(subExpr, currentEnv);
        }
        return result;
    }

    if (op == "set") {
        // set x 10
        // set (prop Point x) 10

        if (expr->expList.at(1)->expType == EvaExpr::ExpType::List) {
            auto& subExprList = expr->expList.at(1)->expList;
            auto setOp = subExprList.at(0)->expString;
            if (setOp == "prop") {
                auto instanceName = subExprList.at(1)->expString;
                auto fieldName = subExprList.at(2)->expString;

                auto instance = _generate(subExprList.at(1), env);
                auto address = _getFieldAddress(instance, fieldName, env);

                // Use GetElementPtr to get the field.
                auto value = _generate(expr->expList.at(2), env);
                return EvaValue{_builder->CreateStore(*value, address)};
            }

            throw std::runtime_error("Unknown set operation: " + setOp);
        }

        auto name = expr->expList.at(1)->expString;
        auto valBinding = env->get(name);
        auto newValue = _generate(expr->expList.at(2), env);
        _builder->CreateStore(*newValue, *valBinding);
        return newValue;
    }

    // (if (== x 10) (print "x is 10") (print "x is not 10"))
    if (op == "if") {
        // if (cond) (then) (else)
        auto cond = _generate(expr->expList.at(1), env);
        auto thenBB = _createBB("then", env->getFunctionScope());
        auto elseBB = _createBB("else", env->getFunctionScope());
        auto ifEndBB = _createBB("ifEnd");

        _builder->CreateCondBr(*cond, thenBB, elseBB);
        _builder->SetInsertPoint(thenBB);
        auto thenRes = _generate(expr->expList.at(2), env);
        _builder->CreateBr(ifEndBB);
        thenBB = _builder->GetInsertBlock();

        _builder->SetInsertPoint(elseBB);
        auto elseRes = _generate(expr->expList.at(3), env);
        _builder->CreateBr(ifEndBB);
        elseBB = _builder->GetInsertBlock();


        auto fn = env->getFunctionScope();
        fn->insert(--fn->end(), ifEndBB);
        _builder->SetInsertPoint(ifEndBB);

        auto phi = _builder->CreatePHI(_builder->getInt32Ty(), 2);
        phi->addIncoming(*thenRes, thenBB);
        phi->addIncoming(*elseRes, elseBB);

        return EvaValue{phi};
    }

    // while (cond) (body)
    // (while (== x 10) (begin (print "x is 10") (set x 0)))
    // cond

    if (op == "while") {
        auto condBB = _createBB("whileCond", env->getFunctionScope());
        auto loopBB = _createBB("whileLoop", env->getFunctionScope());
        auto endLoopBB = _createBB("whileLoopEnd", env->getFunctionScope());

        _builder->CreateBr(condBB);
        _builder->SetInsertPoint(condBB);
        auto cond = _generate(expr->expList.at(1), env);
        _builder->CreateCondBr(*cond, loopBB, endLoopBB);

        _builder->SetInsertPoint(loopBB);
        auto whileRes = _generate(expr->expList.at(2), env);
        _builder->CreateBr(condBB);

        _builder->SetInsertPoint(endLoopBB);
        return EvaValue{_builder->getInt32(0)};
    }

    if (op == "def") {
        // (def add (x) (*x x))
        // (def something ((x number) (y number)) -> number (+ x y))
        auto fnName = env->getClassScope() == nullptr
                              ? expr->expList.at(1)->expString
                              : env->getClassScope()->getStruct()->getName().str() + "_" +
                                        expr->expList.at(1)->expString;
        auto hasReturnType = expr->expList.size() > 4;

        auto returnType = hasReturnType ? _extractType(*expr->expList.at(4), env)
                                        : _extractType(EvaExpr{"number"}, env);

        auto& body = hasReturnType ? expr->expList.at(5) : expr->expList.at(3);


        // (def add (x) (* x x))
        // (def add ((x number) (y number)) -> number (+ x y))
        struct Params {
            std::vector<std::string> names;
            std::vector<EvaType> types;

            [[nodiscard]] int size() const { return names.size(); }
        };

        auto params = [&](const auto& subExpr) {
            Params params;

            if (subExpr->expType == EvaExpr::ExpType::List) {
                for (auto i = 0; i < subExpr->expList.size(); i++) {
                    auto name = extractName(subExpr->expList.at(i));
                    auto type = _extractType(*subExpr->expList.at(i), env);
                    auto& metadata = name;

                    params.names.push_back(name);
                    params.types.push_back(type);
                }
            } else {
                auto name = extractName(subExpr);
                auto type = _extractType(*subExpr, env);
                auto& metadata = name;

                params.names.push_back(name);
                params.types.push_back(type);
            }

            return params;
        }(expr->expList.at(2));

        auto lastBB = _builder->GetInsertBlock();

        std::vector<llvm::Type*> fnTypes;
        std::transform(params.types.begin(), params.types.end(), std::back_inserter(fnTypes),
                       [](const auto& type) { return *type; });

        auto currentFn =
                _createFunction(fnName, llvm::FunctionType::get(*returnType, fnTypes, false), env);

        if (env->getClassScope() != nullptr) {
            env->getClassScope()->insertMethod(expr->expList.at(1)->expString, currentFn);
        }

        auto fnEnv = std::make_shared<EvaEnvironment>(env);
        fnEnv->setFunctionScope(currentFn);

        for (auto i = 0; i < params.size(); i++) {
            auto arg = currentFn->getArg(i);
            arg->setName(params.names[i]);

            // Insert the function arguments into the environment.
            fnEnv->insert(params.names[i], EvaValue{arg, params.types[i]});
        }

        auto retValue = _generate(body, fnEnv);
        auto retFn = _builder->CreateRet(*retValue);
        _builder->SetInsertPoint(lastBB);
        return EvaValue{retFn};
    }

    // (class <name> <parent> <body>)
    // (class Point null
    // (begin
    //    (var x 0)
    //    (var y 0)
    //    (def __init__ (self x y) 0 )
    //    (def calc (self) (+ x y) )
    if (op == "class") {
        auto clsDef = _buildClassDef(expr, env);

        // Set the class scope to the current class definition and generate the class body.
        auto newEnv = std::make_shared<EvaEnvironment>(env);
        newEnv->setClassScope(clsDef.get());

        auto result = _generate(expr->expList.at(3), newEnv);

        auto immutableClsDef = clsDef->toImmutable(*_module);
        _classNameToDefMap[immutableClsDef->getName()] = std::move(immutableClsDef);

        return result;
    }

    // (var point (new Point (10 20)))
    if (op == "new") {
        auto clsName = expr->expList.at(1)->expString;
        auto clsDef = _resolveClass(clsName, env);
        if (!clsDef) {
            throw std::runtime_error("Unknown class: " + clsName);
        }

        auto instanceName = "i_" + clsName;

        auto type = _extractType(EvaExpr{clsName}, env);
        auto instance = _allocateHeapVariable(instanceName, type, env);
        _initClass(instance, *clsDef, *expr->expList.at(2), env);
        return instance;
    }

    // prop point x
    if (op == "prop") {
        auto clsInstanceName = expr->expList.at(1)->expString;
        auto fieldName = expr->expList.at(2)->expString;

        auto instance = _generate(expr->expList.at(1), env);
        auto address = _getFieldAddress(instance, fieldName, env);

        auto clsStrName = instance.type().actualType();
        auto clsDef = _resolveClass(clsStrName, env);

        auto& field = clsDef->getField(fieldName);

        // Use GetElementPtr to get the field.
        return EvaValue{_builder->CreateLoad(*field, address, "v" + fieldName)};
    }


    if (op == "+") {
        BINARY_OP(CreateAdd, "add");
    }
    if (op == "-") {
        BINARY_OP(CreateSub, "sub");
    }
    if (op == "*") {
        BINARY_OP(CreateMul, "mul");
    }
    if (op == "/") {
        BINARY_OP(CreateSDiv, "div");
    }
    if (op == "<") {
        BINARY_OP(CreateICmpSLT, "lt");
    }
    if (op == ">") {
        BINARY_OP(CreateICmpSGT, "gt");
    }
    if (op == "==") {
        BINARY_OP(CreateICmpEQ, "eq");
    }
    if (op == "!=") {
        BINARY_OP(CreateICmpNE, "ne");
    }
    if (op == "<=") {
        BINARY_OP(CreateICmpSLE, "le");
    }
    if (op == ">=") {
        BINARY_OP(CreateICmpSGE, "ge");
    }

    // (method p str)
    // (method p setX 50)
    if (op == "method") {
        auto& clsInstanceName = expr->expList.at(1)->expString;
        auto& methodName = expr->expList.at(2)->expString;

        auto clsInstance = _generate(expr->expList.at(1), env);
        if (clsInstance.isNull()) {
            throw std::runtime_error("Unknown class instance: " + clsInstanceName);
        }

        auto clsName = clsInstance.type().actualType();

        auto clsDef = _resolveClass(clsName, env);
        if (!clsDef) {
            throw std::runtime_error("Unknown class: " + clsName);
        }

        auto clsStr = clsDef->toString();

        auto fnValue = clsDef->getMethodInvocation(*_builder, *clsInstance, methodName);

        std::vector<llvm::Value*> args{*clsInstance};
        for (auto i = 3; i < expr->expList.size(); i++) {
            auto& subExpr = expr->expList.at(i);
            auto arg = _generate(subExpr, env);
            args.push_back(*arg);
        }

        auto* fnType = llvm::dyn_cast<llvm::FunctionType>(*fnValue.type());

        return EvaValue{_builder->CreateCall(fnType, *fnValue, args)};
    }

    // (method self str)
    // (method self setX 50)
    if (op == "super") {
        auto clsDef = env->getClassScope();
        auto clsName = clsDef->getName();

        if (clsDef == nullptr) {
            throw std::runtime_error("Snytax error in class: " + clsName +
                                     ", super is not allowed here");
        }

        auto parentCls = clsDef->getParent();
        if (parentCls == nullptr) {
            throw std::runtime_error("Super class not found for class: " + clsName);
        }

        auto instance = env->get(expr->expList.at(1)->expString);
        if (instance.isNull()) {
            throw std::runtime_error("Unknown instance: " + expr->expList.at(1)->expString +
                                     " for class: " + clsName);
        }

        std::vector<std::unique_ptr<EvaExpr>> initArgs{};
        for (auto i = 3; i < expr->expList.size(); i++) {
            initArgs.push_back(std::move(expr->expList.at(i)));
        }
        EvaExpr initArgsExpr{std::move(initArgs)};

        _initClass(instance, *parentCls, initArgsExpr, env);
        return instance;
    }


    // Function calls.
    // (add 10 20)
    return _handleFunctionCall(op, *expr, env);
}

void EvaLLVM::_initClass(const EvaValue& clsInstance, const EvaClassDef& clsDef,
                         const EvaExpr& initArgs, Env env) const {
    EvaEnvironment newEnv{env};

    auto fnName = clsDef.getName() + "_" + "__init__";
    const auto& initFn = _module->getFunction(fnName);
    if (!initFn) {
        throw std::runtime_error("Cannot find constructor for class : " + clsDef.getName());
    }

    std::vector<llvm::Value*> args{*clsInstance};
    for (auto& subExpr: initArgs.expList) {
        args.push_back(*_generate(subExpr, env));
    }
    _builder->CreateCall(initFn, args);

    auto vTableInstance = clsDef.getVTableInstance(*_module);
    auto vTableAddress = clsDef.getVTableAddress(*_builder, *clsInstance);
    _builder->CreateStore(vTableInstance, vTableAddress);
}

EvaValue EvaLLVM::_handleFunctionCall(const std::string& fnName, const EvaExpr& argsExpr,
                                      Env env) const {
    const auto& callable = _module->getFunction(fnName);
    if (!callable) {
        throw std::runtime_error("Unknown funcation: " + fnName);
    }

    std::vector<llvm::Value*> args{};
    for (auto i = 1; i < argsExpr.expList.size(); i++) {
        auto& subExpr = argsExpr.expList.at(i);
        auto arg = _generate(subExpr, env);
        args.push_back(*arg);
    }

    return EvaValue{_builder->CreateCall(callable, args)};
}

std::shared_ptr<MutableEvaClassDef> EvaLLVM::_buildClassDef(const std::unique_ptr<EvaExpr>& expr,
                                                            Env env) const {
    auto clsName = expr->expList.at(1)->expString;
    auto parentClsName = expr->expList.at(2)->expString;

    if (_classNameToDefMap.contains(clsName)) {
        throw std::runtime_error("Duplicate class definition: " + clsName);
    }

    std::shared_ptr<ImmutableEvaClassDef> parentClsDef = nullptr;
    if (!parentClsName.empty()) {
        if (!_classNameToDefMap.contains(parentClsName)) {
            throw std::runtime_error("Unknown parent class: " + parentClsName);
        }
        parentClsDef = _classNameToDefMap.at(parentClsName);
    }

    auto classDef = std::make_shared<MutableEvaClassDef>(clsName, *_context, parentClsDef);

    auto newEnv = std::make_shared<EvaEnvironment>(env);
    newEnv->setClassScope(classDef.get());

    for (auto& subExpr: expr->expList.at(3)->expList) {
        if (subExpr->expList.at(0)->expString == "var") {
            auto varName = subExpr->expList.at(1)->expString;
            auto varType = _extractType(*subExpr->expList.at(1), newEnv);
            classDef->insertField(varName, *varType);
        }
    }

    return classDef;
}

EvaValue EvaLLVM::_generate(const std::unique_ptr<EvaExpr>& expr, Env env) const {
    switch (expr->expType) {
        case EvaExpr::ExpType::Number:
            return EvaValue{_builder->getInt32(expr->expNumber),
                            EvaType{_builder->getInt32Ty(), "int32"}};

        case EvaExpr::ExpType::String:
            return EvaValue{_builder->CreateGlobalStringPtr(unRawString(expr->expString), "str"),
                            EvaType{_builder->getInt8Ty()->getPointerTo(), "str"}};

        case EvaExpr::ExpType::Symbol: {
            const auto symbol = env->get(expr->expString);

            if (const auto localVar = llvm::dyn_cast<llvm::AllocaInst>(*symbol)) {
                if (localVar->getAllocatedType()->isStructTy()) {
                    // Just return the pointer to the struct if the symbol is a struct and is
                    // already present in the _classes mao.
                    if (_resolveClass(localVar->getAllocatedType()->getStructName().str(), env)) {
                        return EvaValue{localVar, symbol.type()};
                    }

                    throw std::runtime_error(std::string("Unknown allocated class: ") +
                                             localVar->getAllocatedType()->getStructName().data());
                }

                return EvaValue{_builder->CreateLoad(localVar->getAllocatedType(), localVar,
                                                     expr->expString),
                                symbol.type()};
            }

            if (const auto globalVar = llvm::dyn_cast<llvm::GlobalVariable>(*symbol)) {
                return EvaValue{_builder->CreateLoad(globalVar->getInitializer()->getType(),
                                                     globalVar, (*symbol)->getName()),
                                symbol.type()};
            }

            // These are the values passed directly to the function.
            if (const auto argVar = llvm::dyn_cast<llvm::Argument>(*symbol)) {
                return EvaValue{argVar, symbol.type()};
            }

            return EvaValue{_builder->getInt32(0), EvaType{_builder->getInt32Ty(), "int32"}};
        }

        case EvaExpr::ExpType::List: {
            if (const auto& subExpr = expr->expList.front();
                subExpr->expType == EvaExpr::ExpType::Symbol) {
                return _handleOps(expr, env);
            }

            for (const auto& subExpr: expr->expList) {
                std::ignore = _generate(subExpr, env);
            }
            return EvaValue{_builder->getInt32(0), EvaType{_builder->getInt32Ty(), "int32"}};
        }
    }

    return EvaValue{_builder->getInt32(0), EvaType{_builder->getInt32Ty(), "int32"}};
}

llvm::Value* EvaLLVM::_getFieldAddress(const EvaValue& clsInstance, std::string& fieldName,
                                       Env env) const {
    auto clsStrName = clsInstance.type().actualType();

    auto cls = _resolveClass(clsStrName, env);
    if (!cls) {
        throw std::runtime_error("Unknown class: " + clsStrName);
    }
    return cls->getFieldAddress(*_builder, *clsInstance, fieldName);
}

EvaClassDef* EvaLLVM::_resolveClass(const std::string& clsName, Env env) const {
    if (_classNameToDefMap.contains(clsName)) {
        return _classNameToDefMap.at(clsName).get();
    }

    if (env->getClassScope() != nullptr && env->getClassScope()->getName() == clsName) {
        return env->getClassScope();
    }

    return nullptr;
}

llvm::Value* EvaLLVM::_createGlobalVar(const std::string& name, llvm::Constant* init) const {
    _module->getOrInsertGlobal(name, init->getType());
    auto variable = _module->getGlobalVariable(name);
    variable->setInitializer(init);
    variable->setAlignment(llvm::MaybeAlign(4));
    return variable;
}

llvm::Function* EvaLLVM::_createFunction(const std::string& name, llvm::FunctionType* fnType,
                                         Env env) const {
    auto fn = _module->getFunction(name);
    if (!fn) {
        fn = _createFunctionProto(name, fnType, env);
    }

    _createFunctionBlock(fn);
    return fn;
}

llvm::Function* EvaLLVM::_createFunctionProto(const std::string& name, llvm::FunctionType* fnType,
                                              Env env) const {
    auto fn = llvm::Function::Create(fnType, llvm::Function::ExternalLinkage, name, *_module);
    env->insert(name, EvaValue{fn});
    return fn;
}

void EvaLLVM::_createFunctionBlock(llvm::Function* fn) const {
    auto entry = _createBB("entry", fn);
    _builder->SetInsertPoint(entry);
}

llvm::BasicBlock* EvaLLVM::_createBB(const std::string& name, llvm::Function* fn) const {
    return llvm::BasicBlock::Create(*_context, name, fn);
}

llvm::Value* EvaLLVM::_allocateStackVariable(llvm::Function* fn, const std::string& name,
                                             EvaType type, const Env env) const {
    const auto currentBuilder = std::make_unique<llvm::IRBuilder<>>(*_context);
    currentBuilder->SetInsertPoint(&fn->getEntryBlock(), fn->getEntryBlock().begin());
    const auto allocVar = currentBuilder->CreateAlloca(*type, nullptr, name);
    env->insert(name, EvaValue{allocVar, type});
    return allocVar;
}

EvaValue EvaLLVM::_allocateHeapVariable(const std::string& name, EvaType type,
                                        const Env env) const {
    llvm::DataLayout dataLayout{_module.get()};
    auto structSize = dataLayout.getTypeAllocSize(*type);

    // Create a constant for the size
    llvm::Value* allocSize = llvm::ConstantInt::get(_builder->getInt64Ty(), structSize);

    auto gcMallocFn = _module->getFunction("GC_malloc");
    auto allocVar = _builder->CreateCall(gcMallocFn, allocSize);
    env->insert(name, EvaValue{allocVar, type});
    return EvaValue{allocVar, type};
}

void EvaLLVM::_setupExternalFunctions() const {
    _module->getOrInsertFunction(
            "printf", llvm::FunctionType::get(_builder->getInt32Ty(),
                                              _builder->getInt8Ty()->getPointerTo(), true));

    _module->getOrInsertFunction("GC_malloc",
                                 llvm::FunctionType::get(_builder->getInt8Ty()->getPointerTo(),
                                                         _builder->getInt64Ty(), false));
}

void EvaLLVM::_setupGlobalEnviroment() const {
    const std::unordered_map<std::string, llvm::Value*> globals{
            {"VERSION", _builder->getInt32(1001)}};

    std::unordered_map<std::string, EvaValue> globalVars{};
    for (auto& [name, value]: globals) {
        EvaValue typedValue{_createGlobalVar(name, llvm::dyn_cast<llvm::Constant>(value))};
        globalVars[name] = typedValue;
    }
    _globalEnv = std::make_shared<EvaEnvironment>(globalVars, nullptr);
}


void EvaLLVM::_saveModule(const std::string& filename) const {
    std::error_code error;
    llvm::raw_fd_ostream file(filename, error);
    _module->print(file, nullptr);
}

int main(int argc, const char* argv[]) {
    if (argc < 2) {
        std::cerr << "Usage: " << argv[0] << " <file_path>" << std::endl;
        return 1;
    }

    std::string fileName = argv[1];
    std::ifstream file(fileName);
    if (!file.is_open()) {
        std::cerr << "Unable to open file: " << fileName << std::endl;
        return 1;
    }

    std::ostringstream program;
    program << file.rdbuf();
    file.close();

    EvaLLVM vm{};
    vm.testSample();
    vm.exec(program.str(), "eva.ll");
    return 0;
}

void EvaLLVM::testSample() const {
    llvm::LLVMContext context;
    llvm::Module module("struct_example", context);
    llvm::IRBuilder<> builder(context);

    // Define the structure type with two fields: an int and a float
    std::vector<llvm::Type*> structFields;
    structFields.push_back(builder.getInt32Ty()); // int field
    structFields.push_back(builder.getFloatTy()); // float field

    llvm::StructType* structType = llvm::StructType::create(context, structFields, "MyStruct");

    // Create a function that allocates the struct and accesses its fields
    llvm::FunctionType* funcType = llvm::FunctionType::get(builder.getVoidTy(), false);
    llvm::Function* func =
            llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, "main", &module);
    llvm::BasicBlock* entry = llvm::BasicBlock::Create(context, "entry", func);
    builder.SetInsertPoint(entry);

    // Allocate memory for the struct
    llvm::Value* structAlloc = builder.CreateAlloca(structType, nullptr, "myStruct");

    // Get the pointer to the first field
    llvm::Value* firstFieldPtr =
            builder.CreateStructGEP(structType, structAlloc, 0, "firstFieldPtr");
    llvm::Value* secondFieldPtr =
            builder.CreateStructGEP(structType, structAlloc, 1, "secondFieldPtr");

    // Store values in the struct fields
    builder.CreateStore(builder.getInt32(42), firstFieldPtr);
    builder.CreateStore(llvm::ConstantFP::get(context, llvm::APFloat(3.14f)), secondFieldPtr);

    // Load the values from the struct fields
    llvm::Value* firstFieldVal =
            builder.CreateLoad(builder.getInt32Ty(), firstFieldPtr, "firstFieldVal");
    llvm::Value* secondFieldVal =
            builder.CreateLoad(builder.getFloatTy(), secondFieldPtr, "secondFieldVal");

    // Finish the function
    builder.CreateRetVoid();

    // Verify the module
    llvm::verifyModule(module, &llvm::errs());

    // Write the generated LLVM IR to a file
    std::error_code errorCode;
    llvm::raw_fd_ostream file("output.ll", errorCode);
    if (errorCode) {
        llvm::errs() << "Error opening file: " << errorCode.message() << "\n";
    }

    module.print(file, nullptr);
}
