#include "evaLLM.h"
#include <fstream>
#include <sstream>
#include "evaParser.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"

extern int yydebug;

#define BINARY_OP(OP, NAME, CLS)                             \
    auto lhs = _generate(expr->expList.at(1), env, fn, cls); \
    auto rhs = _generate(expr->expList.at(2), env, fn, cls); \
    return _builder->OP(lhs, rhs, NAME);

namespace {

    auto extractName = [](const auto& subExpr) -> std::string {
        if (subExpr->expType == EvaExpr::ExpType::List) {
            return subExpr->expList.at(0)->expString;
        }
        return subExpr->expString;
    };

    auto extractType = [](const auto& subExpr) -> std::string {
        if (subExpr->expType == EvaExpr::ExpType::List) {
            if (subExpr->expList.at(0)->expString == "self") {
                return "self";
            }
            return subExpr->expList.at(1)->expString;
        }
        return subExpr->expString == "self" ? "self" : "number";
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


EvaLLM::EvaLLM() {
    _context = std::make_unique<llvm::LLVMContext>();
    _module = std::make_unique<llvm::Module>("eva", *_context);
    _builder = std::make_unique<llvm::IRBuilder<>>(*_context);

    yydebug = 1;

    _setupExternalFunctions();
    _setupGlobalEnviroment();
}

void EvaLLM::exec(const std::string& program, const std::string& outputFilename) const {
    const auto parser = std::make_unique<EvaParser>();

    _compile(std::move(parser->parse(program.c_str())));
    _saveModule(outputFilename);
}

void EvaLLM::_compile(std::unique_ptr<EvaExpr> expr) const {
    llvm::Function* mainFn = _createFunction(
            "main", llvm::FunctionType::get(_builder->getInt32Ty(), false), _globalEnv);

    auto result = _generate(expr, _globalEnv, mainFn, nullptr);
    result = _builder->CreateIntCast(result, _builder->getInt32Ty(), true);
    _builder->CreateRet(result);

    verifyFunction(*mainFn);
}

llvm::Type* EvaLLM::toType(const std::string& type, llvm::StructType* cls) const {
    if (type == "self") {
        if (!cls) {
            throw std::runtime_error("self is not allowed here");
        }
        return cls->getPointerTo();
    }

    if (type == "number") {
        return _builder->getInt32Ty();
    }
    if (type == "string") {
        return _builder->getInt8Ty()->getPointerTo();
    }

    if (_classes.contains(type)) {
        auto clsDef = _classes.at(type);
        return clsDef->cls;
    }

    return _builder->getInt32Ty();
}

llvm::Value* EvaLLM::handleOps(const std::unique_ptr<EvaExpr>& expr, Env env, llvm::Function* fn,
                               llvm::StructType* cls) const {
    auto op = expr->expList.front()->expString;

    if (op == "print") {
        const auto& printFn = _module->getFunction("printf");
        std::vector<llvm::Value*> args{};
        for (auto i = 1; i < expr->expList.size(); i++) {
            auto& subExpr = expr->expList.at(i);
            auto arg = _generate(subExpr, env, fn, cls);
            args.push_back(arg);
        }
        return _builder->CreateCall(printFn, args);
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
        const auto& type = toType(extractType(expr->expList.at(1)), cls);

        const auto& subExpr = expr->expList.at(2);
        const auto& init = _generate(subExpr, env, fn, cls);

        auto varBinding = allocateVariable(fn, name, type, env);
        _builder->CreateStore(init, varBinding);

        return init;
    }

    if (op == "begin") {
        auto currentEnv = std::make_shared<EvaEnvironment>(env);
        llvm::Value* result = nullptr;
        for (auto i = 1; i < expr->expList.size(); i++) {
            auto& subExpr = expr->expList.at(i);
            result = _generate(subExpr, currentEnv, fn, cls);
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
                auto clsName = subExprList.at(1)->expString;
                auto instance = _generate(subExprList.at(1), env, fn, cls);
                auto instancePtr = llvm::dyn_cast<llvm::AllocaInst>(instance);
                auto clsType = instancePtr->getAllocatedType(); // Struct actual type.

                // The instance will of actual class type, but we need to get the pointer to the
                // class type.
                // auto instancePtr = _builder->CreateAlloca(clsType, nullptr);
                //_builder->CreateStore(instance, instancePtr);

                auto clsDef = _classes.at(clsType->getStructName().data());
                if (!clsDef) {
                    throw std::runtime_error("Unknown class: " + clsName);
                }
                auto fieldName = subExprList.at(2)->expString;
                if (!clsDef->fields.contains(fieldName)) {
                    throw std::runtime_error("Unknown field: " + fieldName);
                }

                auto field = clsDef->fields.at(fieldName);
                auto fieldIndex = field.first;

                auto address = _builder->CreateStructGEP(clsType, instancePtr, fieldIndex);

                // Use GetElementPtr to get the field.
                auto newValue = _generate(expr->expList.at(2), env, fn, cls);
                return _builder->CreateStore(newValue, address);
            }

            throw std::runtime_error("Unknown set operation: " + setOp);
        }

        auto name = expr->expList.at(1)->expString;
        auto valBinding = env->get(name);
        auto newValue = _generate(expr->expList.at(2), env, fn, cls);
        _builder->CreateStore(newValue, valBinding);
        return newValue;
    }

    // (if (== x 10) (print "x is 10") (print "x is not 10"))
    if (op == "if") {
        // if (cond) (then) (else)
        auto cond = _generate(expr->expList.at(1), env, fn, cls);
        auto thenBB = _createBB("then", fn);
        auto elseBB = _createBB("else", fn);
        auto ifEndBB = _createBB("ifEnd");

        _builder->CreateCondBr(cond, thenBB, elseBB);
        _builder->SetInsertPoint(thenBB);
        auto thenRes = _generate(expr->expList.at(2), env, fn, cls);
        _builder->CreateBr(ifEndBB);
        thenBB = _builder->GetInsertBlock();

        _builder->SetInsertPoint(elseBB);
        auto elseRes = _generate(expr->expList.at(3), env, fn, cls);
        _builder->CreateBr(ifEndBB);
        elseBB = _builder->GetInsertBlock();


        fn->insert(--fn->end(), ifEndBB);
        _builder->SetInsertPoint(ifEndBB);

        auto phi = _builder->CreatePHI(_builder->getInt32Ty(), 2);
        phi->addIncoming(thenRes, thenBB);
        phi->addIncoming(elseRes, elseBB);

        return phi;
    }

    // while (cond) (body)
    // (while (== x 10) (begin (print "x is 10") (set x 0)))
    // cond

    if (op == "while") {
        auto condBB = _createBB("whileCond", fn);
        auto loopBB = _createBB("whileLoop", fn);
        auto endLoopBB = _createBB("whileLoopEnd", fn);

        _builder->CreateBr(condBB);
        _builder->SetInsertPoint(condBB);
        auto cond = _generate(expr->expList.at(1), env, fn, cls);
        _builder->CreateCondBr(cond, loopBB, endLoopBB);

        _builder->SetInsertPoint(loopBB);
        auto whileRes = _generate(expr->expList.at(2), env, fn, nullptr);
        _builder->CreateBr(condBB);

        _builder->SetInsertPoint(endLoopBB);
        return _builder->getInt32(0);
    }

    if (op == "def") {
        // (def add (x) (*x x))
        // (def something ((x number) (y number)) -> number (+ x y))
        auto fnName = cls == nullptr ? expr->expList.at(1)->expString
                                     : cls->getName().str() + "_" + expr->expList.at(1)->expString;
        auto hasReturnType = expr->expList.size() > 4;

        auto returnType = hasReturnType ? toType(extractType(expr->expList.at(4)), cls)
                                        : toType("number", cls);

        auto& body = hasReturnType ? expr->expList.at(5) : expr->expList.at(3);


        // (def add (x) (* x x))
        // (def add ((x number) (y number)) -> number (+ x y))
        auto extractParams = [&](const auto& subExpr) {
            std::vector<llvm::Type*> paramsType;
            std::vector<std::string> paramsName;
            if (subExpr->expType == EvaExpr::ExpType::List) {
                for (auto i = 0; i < subExpr->expList.size(); i++) {
                    paramsType.push_back(toType(extractType(subExpr->expList.at(i)), cls));
                    paramsName.push_back(extractName(subExpr->expList.at(i)));
                }
            } else {
                paramsType.push_back(toType(extractType(subExpr), cls));
                paramsName.push_back(extractName(subExpr));
            }
            return std::make_pair(std::move(paramsType), std::move(paramsName));
        };


        auto params = extractParams(expr->expList.at(2));

        auto lastBB = _builder->GetInsertBlock();

        auto currentFn = _createFunction(
                fnName, llvm::FunctionType::get(returnType, params.first, false), env);

        auto fnEnv = std::make_shared<EvaEnvironment>(env);

        for (auto i = 0; i < params.second.size(); i++) {
            auto arg = currentFn->getArg(i);
            arg->setName(params.second[i]);
            auto varBinding = allocateVariable(currentFn, params.second[i], params.first[i], fnEnv);
            _builder->CreateStore(arg, varBinding);
        }

        auto retFn = _builder->CreateRet(_generate(body, fnEnv, currentFn, cls));
        _builder->SetInsertPoint(lastBB);
        return retFn;
    }

    // (class <name> <parent> <body>)
    // (class Point null
    // (begin
    //    (var x 0)
    //    (var y 0)
    //    (def __init__ (self x y) 0 )
    //    (def calc (self) (+ x y) )
    if (op == "class") {
        auto clsDef = _buildClassDef(expr);
        return _generate(expr->expList.at(3), env, fn, clsDef->cls);
    }

    // (var point (new Point (10 20)))
    if (op == "new") {
        auto clsName = expr->expList.at(1)->expString;
        auto clsDef = _classes.at(clsName);
        if (!clsDef) {
            throw std::runtime_error("Unknown class: " + clsName);
        }

        auto instanceName = "i" + clsName;
        auto instance = _builder->CreateAlloca(clsDef->cls, nullptr, instanceName);
        // auto clsSize = _module->getDataLayout().getTypeAllocSize(clsDef->cls);
        // auto malloc = _module->getFunction("GC_malloc");
        // auto instancePtr = _builder->CreateCall(malloc, _builder->getInt64(clsSize));
        // auto instance = _builder->CreatePointerCast(instancePtr, clsDef->cls->getPointerTo());

        auto constructorName = clsDef->name + "_" + "__init__";
        const auto& constructor = _module->getFunction(constructorName);
        if (!constructor) {
            throw std::runtime_error("Cannot find constructor for class : " + clsName);
        }

        std::vector<llvm::Value*> args{instance};
        for (auto& subExpr: expr->expList.at(2)->expList) {
            args.push_back(_generate(subExpr, env, fn, cls));
        }

        _builder->CreateCall(constructor, args);
        return instance;
    }

    // prop point x
    if (op == "prop") {
        auto clsName = expr->expList.at(1)->expString;
        auto instance = _generate(expr->expList.at(1), env, fn, cls);
        auto instancePtr = llvm::dyn_cast<llvm::AllocaInst>(instance);

        auto cls = instancePtr->getAllocatedType(); // Struct actual type.

        // The instance will of actual class type, but we need to get the pointer to the class type.
        // auto instancePtr = _builder->CreateAlloca(cls, nullptr, "ptr" + clsName);
        // _builder->CreateStore(instance, instancePtr);

        auto clsDef = _classes.at(cls->getStructName().data());
        if (!clsDef) {
            throw std::runtime_error("Unknown class: " + clsName);
        }
        auto fieldName = expr->expList.at(2)->expString;
        if (!clsDef->fields.contains(fieldName)) {
            throw std::runtime_error("Unknown field: " + fieldName);
        }

        auto field = clsDef->fields.at(fieldName);
        auto fieldIndex = field.first;

        auto address = _builder->CreateStructGEP(cls, instancePtr, fieldIndex, "p" + fieldName);
        // Use GetElementPtr to get the field.
        return _builder->CreateLoad(field.second, address, "v" + fieldName);
    }


    if (op == "+") {
        BINARY_OP(CreateAdd, "add", cls);
    }
    if (op == "-") {
        BINARY_OP(CreateSub, "sub", cls);
    }
    if (op == "*") {
        BINARY_OP(CreateMul, "mul", cls);
    }
    if (op == "/") {
        BINARY_OP(CreateSDiv, "div", cls);
    }
    if (op == "<") {
        BINARY_OP(CreateICmpSLT, "lt", cls);
    }
    if (op == ">") {
        BINARY_OP(CreateICmpSGT, "gt", cls);
    }
    if (op == "==") {
        BINARY_OP(CreateICmpEQ, "eq", cls);
    }
    if (op == "!=") {
        BINARY_OP(CreateICmpNE, "ne", cls);
    }
    if (op == "<=") {
        BINARY_OP(CreateICmpSLE, "le", cls);
    }
    if (op == ">=") {
        BINARY_OP(CreateICmpSGE, "ge", cls);
    }

    const auto& callable = _module->getFunction(op);
    if (!callable) {
        throw std::runtime_error("Unknown callable: " + op);
    }

    std::vector<llvm::Value*> args{};
    for (auto i = 1; i < expr->expList.size(); i++) {
        auto& subExpr = expr->expList.at(i);
        auto arg = _generate(subExpr, env, fn, cls);
        args.push_back(arg);
    }
    return _builder->CreateCall(callable, args);
}


std::shared_ptr<EvaLLM::ClassDef> EvaLLM::_buildClassDef(
        const std::unique_ptr<EvaExpr>& expr) const {
    auto name = expr->expList.at(1)->expString;
    auto parent = expr->expList.at(2)->expString;

    if (_classes.contains(name)) {
        throw std::runtime_error("Duplicate class definition: " + name);
    }

    _classes[name] = std::make_shared<ClassDef>(name, *_context);
    auto classDef = _classes[name];
    auto& cls = classDef->cls;

    int i = 0;
    for (auto& subExpr: expr->expList.at(3)->expList) {
        if (subExpr->expList.at(0)->expString == "var") {
            auto varName = subExpr->expList.at(1)->expString;
            auto varType = toType(extractType(subExpr->expList.at(1)), cls);
            classDef->fields[varName] = std::make_pair(i++, varType);
        }
    }

    std::vector<llvm::Type*> fieldsType;
    for (const auto& [name, indexType]: classDef->fields) {
        auto& [_, type] = indexType;
        fieldsType.push_back(type);
    }

    cls->setBody(fieldsType, false);

    return classDef;
}

llvm::Value* EvaLLM::_generate(const std::unique_ptr<EvaExpr>& expr, Env env, llvm::Function* fn,
                               llvm::StructType* cls) const {
    switch (expr->expType) {
        case EvaExpr::ExpType::Number:
            return _builder->getInt32(expr->expNumber);

        case EvaExpr::ExpType::String:
            return _builder->CreateGlobalStringPtr(unRawString(expr->expString));

        case EvaExpr::ExpType::Symbol: {
            const auto symbol = env->get(expr->expString);

            if (const auto localVar = llvm::dyn_cast<llvm::AllocaInst>(symbol)) {
                if (localVar->getAllocatedType()->isStructTy()) {
                    // Just return the pointer to the struct if the symbol is a struct and is
                    // already present in the _classes mao.
                    if (_classes.contains(localVar->getAllocatedType()->getStructName().data())) {
                        return localVar;
                    }
                }

                return _builder->CreateLoad(localVar->getAllocatedType(), localVar,
                                            expr->expString);
            }

            if (const auto globalVar = llvm::dyn_cast<llvm::GlobalVariable>(symbol)) {
                return _builder->CreateLoad(globalVar->getInitializer()->getType(), globalVar,
                                            symbol->getName());
            }
            return _builder->getInt32(0);
        }
        case EvaExpr::ExpType::List: {
            if (const auto& subExpr = expr->expList.front();
                subExpr->expType == EvaExpr::ExpType::Symbol) {
                return handleOps(expr, env, fn, cls);
            }

            for (const auto& subExpr: expr->expList) {
                std::ignore = _generate(subExpr, env, fn, cls);
            }
            return _builder->getInt32(0);
        }
    }

    return _builder->getInt32(0);
}

llvm::Value* EvaLLM::_createGlobalVar(const std::string& name, llvm::Constant* init) const {
    _module->getOrInsertGlobal(name, init->getType());
    auto variable = _module->getGlobalVariable(name);
    variable->setInitializer(init);
    variable->setAlignment(llvm::MaybeAlign(4));
    return variable;
}

llvm::Function* EvaLLM::_createFunction(const std::string& name, llvm::FunctionType* fnType,
                                        Env env) const {
    auto fn = _module->getFunction(name);
    if (!fn) {
        fn = _createFunctionProto(name, fnType, env);
    }

    _createFunctionBlock(fn);
    return fn;
}

llvm::Function* EvaLLM::_createFunctionProto(const std::string& name, llvm::FunctionType* fnType,
                                             Env env) const {
    auto fn = llvm::Function::Create(fnType, llvm::Function::ExternalLinkage, name, *_module);
    env->insert(name, fn);
    return fn;
}

void EvaLLM::_createFunctionBlock(llvm::Function* fn) const {
    auto entry = _createBB("entry", fn);
    _builder->SetInsertPoint(entry);
}

llvm::BasicBlock* EvaLLM::_createBB(const std::string& name, llvm::Function* fn) const {
    return llvm::BasicBlock::Create(*_context, name, fn);
}

llvm::Value* EvaLLM::allocateVariable(llvm::Function* fn, const std::string& name, llvm::Type* type,
                                      const Env env) const {
    const auto currentBuilder = std::make_unique<llvm::IRBuilder<>>(*_context);
    currentBuilder->SetInsertPoint(&fn->getEntryBlock(), fn->getEntryBlock().begin());
    const auto allocVar = currentBuilder->CreateAlloca(type, nullptr, name);
    env->insert(name, allocVar);
    return allocVar;
}

void EvaLLM::_setupExternalFunctions() const {
    _module->getOrInsertFunction(
            "printf", llvm::FunctionType::get(_builder->getInt32Ty(),
                                              _builder->getInt8Ty()->getPointerTo(), true));

    _module->getOrInsertFunction("GC_malloc",
                                 llvm::FunctionType::get(_builder->getInt8Ty()->getPointerTo(),
                                                         _builder->getInt64Ty(), false));
}

void EvaLLM::_setupGlobalEnviroment() const {
    const std::unordered_map<std::string, llvm::Value*> globals{
            {"VERSION", _builder->getInt32(1001)}};

    std::unordered_map<std::string, llvm::Value*> globalVars{};
    for (auto& [name, value]: globals) {
        globalVars[name] = _createGlobalVar(name, llvm::dyn_cast<llvm::Constant>(value));
    }
    _globalEnv = std::make_shared<EvaEnvironment>(globalVars, nullptr);
}


void EvaLLM::_saveModule(const std::string& filename) const {
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

    EvaLLM vm{};
    vm.testSample();
    vm.exec(program.str(), "eva.ll");
    return 0;
}

void EvaLLM::testSample() const {
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
