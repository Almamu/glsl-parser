#include <cstdio>
#include <cstring>

#include "printer.h"
#include <string>

namespace glsl {

#undef KEYWORD
#define KEYWORD(X) #X,
    static const char *kTypes[] = {
#include "lexemes.h"
    };
#undef KEYWORD
#define KEYWORD(...)

#undef OPERATOR
#define OPERATOR(N, S, P) S,
    static const char *kOperators[] = {
#include "lexemes.h"
    };
#undef OPERATOR
#define OPERATOR(...)

    std::string buffer;

    static void output(const char* format, ...) {
        va_list args;
        va_start(args, format);

        int size = vsnprintf(nullptr, 0, format, args);

        if (size < 0) {
            va_end(args);
            return;
        }

        size_t oldSize = buffer.size();
        buffer.resize(oldSize + size);

        va_start (args, format); // restart args
        vsnprintf(&buffer[oldSize], size + 1, format, args);
        va_end(args);
    }

    static void printExpression(astExpression *expression);
    static void printStatement(astStatement *statement);
    static void printNodes(const vector<astBase*>& nodes);

    static void printBuiltin(astBuiltin *builtin) {
        output("%s", kTypes[builtin->type]);
    }

    static void printType(astType *type) {
        if (type->typeType == astType::kBuiltin)
            printBuiltin((astBuiltin*)type);
        else
            output("%s", ((astStruct *) type)->name);
    }

    static void printIntConstant(astIntConstant *expression) {
        output("%d", expression->value);
    }

    static void printUIntConstant(astUIntConstant *expression) {
        output("%du", expression->value);
    }

    static void printFloatConstant(astFloatConstant *expression) {
        char format[1024];
        snprintf(format, sizeof format, "%g", expression->value);
        if (!strchr(format, '.'))
            output("%g.0", expression->value);
        else
            output("%s", format);
    }

    static void printDoubleConstant(astDoubleConstant *expression) {
        output("%g", expression->value);
    }

    static void printBoolConstant(astBoolConstant *expression) {
        output("%s", expression->value ? "true" : "false");
    }

    static void printArraySize(const vector<astConstantExpression*> &arraySizes) {
        for (auto arraySize : arraySizes) {
            output("[");
            printExpression(arraySize);
            output("]");
        }
    }

    static void printVariable(astVariable *variable, bool nameOnly = false) {
        if (variable->isPrecise)
            output("precise ");

        if (nameOnly) {
            output("%s", variable->name);
            return;
        }

        printType(variable->baseType);
        output(" %s", variable->name);

        if (nameOnly)
            return;

        if (variable->isArray)
            printArraySize(variable->arraySizes);
    }

    static void printStorage(int storage) {
        switch (storage) {
            case kConst:
                output("const ");
                break;
            case kIn:
                output("in ");
                break;
            case kOut:
                output("out ");
                break;
            case kAttribute:
                output("attribute ");
                break;
            case kUniform:
                output("uniform ");
                break;
            case kVarying:
                output("varying ");
                break;
            case kBuffer:
                output("buffer ");
                break;
            case kShared:
                output("shared ");
                break;
            default:
                break;
        }
    }

    static void printAuxiliary(int auxiliary) {
        switch (auxiliary) {
            case kCentroid:
                output("centroid ");
                break;
            case kSample:
                output("sample ");
                break;
            case kPatch:
                output("patch ");
                break;
            default:
                break;
        }
    }

    static void printMemory(int memory) {
        if (memory & kCoherent) output("coherent ");
        if (memory & kVolatile) output("volatile ");
        if (memory & kRestrict) output("restrict ");
        if (memory & kReadOnly) output("readonly ");
        if (memory & kWriteOnly) output("writeonly ");
    }

    static void printPrecision(int precision) {
        switch (precision) {
            case kLowp:
                output("lowp ");
                break;
            case kMediump:
                output("mediump ");
                break;
            case kHighp:
                output("highp ");
                break;
            default:
                break;
        }
    }

    static void printGlobalVariable(astGlobalVariable *variable) {
        vector<astLayoutQualifier*> &qualifiers = variable->layoutQualifiers;
        if (!variable->layoutQualifiers.empty()) {
            output("layout (");
            for (size_t i = 0; i < qualifiers.size(); i++) {
                astLayoutQualifier *qualifier = qualifiers[i];
                output("%s", qualifier->name);
                if (qualifier->initialValue) {
                    output(" = ");
                    printExpression(qualifier->initialValue);
                }
                if (i != qualifiers.size() - 1)
                    output(", ");
            }
            output(") ");
        }

        printStorage(variable->storage);
        printAuxiliary(variable->auxiliary);
        printMemory(variable->memory);
        printPrecision(variable->precision);

        if (variable->isInvariant)
            output("invariant ");

        switch (variable->interpolation) {
            case kSmooth:
                output("smooth ");
                break;
            case kFlat:
                output("flat ");
                break;
            case kNoPerspective:
                output("noperspective ");
                break;
        }

        printVariable((astVariable*)variable);

        if (variable->initialValue) {
            output(" = ");
            printExpression(variable->initialValue);
        }

        output(";\n");
    }

    static void printVariableIdentifier(astVariableIdentifier *expression) {
        printVariable(expression->variable, true);
    }

    static void printDefineIdentifier(astDefineIdentifier *expression) {
        output("%s", expression->define->name);
    }

    static void printUnknownIdentifier(astUnknownIdentifier *expression) {
        output("%s", expression->define);
    }

    static void printFieldOrSwizzle(astFieldOrSwizzle *expression) {
        printExpression(expression->operand);
        output(".%s", expression->name);
    }

    static void printArraySubscript(astArraySubscript *expression) {
        printExpression(expression->operand);
        output("[");
        printExpression(expression->index);
        output("]");
    }

    static void printFunctionCall(astFunctionCall *expression) {
        output("%s(", expression->name);
        for (size_t i = 0; i < expression->parameters.size(); i++) {
            printExpression(expression->parameters[i]);
            if (i != expression->parameters.size() - 1)
                output(", ");
        }
        output(")");
    }

    static void printConstructorCall(astConstructorCall *expression) {
        printType(expression->type);
        output("(");
        for (size_t i = 0; i < expression->parameters.size(); i++) {
            printExpression(expression->parameters[i]);
            if (i != expression->parameters.size() - 1)
                output(", ");
        }
        output(")");
    }

    enum { kSemicolon = 1 << 0, kNewLine = 1 << 1, kDefault = kSemicolon | kNewLine };

    static void printFunctionVariable(astFunctionVariable *variable, int flags = kDefault ) {
        if (variable->isConst)
            output("const ");
        printVariable((astVariable*)variable);
        if (variable->initialValue) {
            output(" = ");
            printExpression(variable->initialValue);
        }
        if (flags & kSemicolon) output(";");
        if (flags & kNewLine) output("\n");
    }

    static void printPostIncrement(astPostIncrementExpression *expression) {
        printExpression(expression->operand);
        output("++");
    }

    static void printPostDecrement(astPostDecrementExpression *expression) {
        printExpression(expression->operand);
        output("--");
    }

    static void printUnaryMinus(astUnaryMinusExpression *expression) {
        output("-");
        printExpression(expression->operand);
    }

    static void printUnaryPlus(astUnaryPlusExpression *expression) {
        output("+");
        printExpression(expression->operand);
    }

    static void printUnaryBitNot(astUnaryBitNotExpression *expression) {
        output("~");
        printExpression(expression->operand);
    }

    static void printUnaryLogicalNot(astUnaryLogicalNotExpression *expression) {
        output("!");
        printExpression(expression->operand);
    }

    static void printPrefixIncrement(astPrefixIncrementExpression *expression) {
        output("++");
        printExpression(expression->operand);
    }

    static void printPrefixDecrement(astPrefixDecrementExpression *expression) {
        output("--");
        printExpression(expression->operand);
    }

    static void printAssign(astAssignmentExpression *expression) {
        printExpression(expression->operand1);
        output(" %s ", kOperators[expression->assignment]);
        printExpression(expression->operand2);
    }

    static void printSequence(astSequenceExpression *expression) {
        output("(");
        printExpression(expression->operand1);
        output(", ");
        printExpression(expression->operand2);
        output(")");
    }

    static void printOperation(astOperationExpression *expression) {
        output("(");
        printExpression(expression->operand1);
        output(" %s ", kOperators[expression->operation]);
        printExpression(expression->operand2);
        output(")");
    }

    static void printTernary(astTernaryExpression *expression) {
        output("(");
        printExpression(expression->condition);
        output(" ? ");
        printExpression(expression->onTrue);
        output(" : ");
        printExpression(expression->onFalse);
        output(")");
    }

    static void printExpression(astExpression *expression) {
        switch (expression->type) {
            case astExpression::kIntConstant:
                return printIntConstant((astIntConstant*)expression);
            case astExpression::kUIntConstant:
                return printUIntConstant((astUIntConstant*)expression);
            case astExpression::kFloatConstant:
                return printFloatConstant((astFloatConstant*)expression);
            case astExpression::kDoubleConstant:
                return printDoubleConstant((astDoubleConstant*)expression);
            case astExpression::kBoolConstant:
                return printBoolConstant((astBoolConstant*)expression);
            case astExpression::kVariableIdentifier:
                return printVariableIdentifier((astVariableIdentifier*)expression);
            case astExpression::kDefineIdentifier:
                return printDefineIdentifier((astDefineIdentifier*)expression);
            case astExpression::kUnknownIdentifier:
                return printUnknownIdentifier((astUnknownIdentifier*)expression);
            case astExpression::kFieldOrSwizzle:
                return printFieldOrSwizzle((astFieldOrSwizzle*)expression);
            case astExpression::kArraySubscript:
                return printArraySubscript((astArraySubscript*)expression);
            case astExpression::kFunctionCall:
                return printFunctionCall((astFunctionCall*)expression);
            case astExpression::kConstructorCall:
                return printConstructorCall((astConstructorCall*)expression);
            case astExpression::kPostIncrement:
                return printPostIncrement((astPostIncrementExpression*)expression);
            case astExpression::kPostDecrement:
                return printPostDecrement((astPostDecrementExpression*)expression);
            case astExpression::kUnaryMinus:
                return printUnaryMinus((astUnaryMinusExpression*)expression);
            case astExpression::kUnaryPlus:
                return printUnaryPlus((astUnaryPlusExpression*)expression);
            case astExpression::kBitNot:
                return printUnaryBitNot((astUnaryBitNotExpression*)expression);
            case astExpression::kLogicalNot:
                return printUnaryLogicalNot((astUnaryLogicalNotExpression*)expression);
            case astExpression::kPrefixIncrement:
                return printPrefixIncrement((astPrefixIncrementExpression*)expression);
            case astExpression::kPrefixDecrement:
                return printPrefixDecrement((astPrefixDecrementExpression*)expression);
            case astExpression::kAssign:
                return printAssign((astAssignmentExpression*)expression);
            case astExpression::kSequence:
                return printSequence((astSequenceExpression*)expression);
            case astExpression::kOperation:
                return printOperation((astOperationExpression*)expression);
            case astExpression::kTernary:
                return printTernary((astTernaryExpression*)expression);
        }
    }

    static void printCompoundStatement(astCompoundStatement *statement) {
        output(" {\n");
        for (auto & i : statement->statements)
            printStatement(i);
        output("}\n");
    }

    static void printEmptyStatement() {
        output(";");
    }

    static void printDeclarationStatement(astDeclarationStatement *statement, int flags = kDefault) {
        for (auto & variable : statement->variables)
            printFunctionVariable(variable, flags);
    }

    static void printExpressionStatement(astExpressionStatement *statement, int flags = kDefault) {
        printExpression(statement->expression);
        if (flags & kSemicolon) output(";");
        if (flags & kNewLine) output("\n");
    }

    static void printIfStetement(astIfStatement *statement) {
        output("if(");
        printExpression(statement->condition);
        output(")");
        printStatement(statement->thenStatement);
        if (statement->elseStatement) {
            output("else");
            if (statement->elseStatement->type == astStatement::kIf)
                output(" ");
            printStatement(statement->elseStatement);
        }
    }

    static void printSwitchStatement(astSwitchStatement *statement) {
        output("switch(");
        printExpression(statement->expression);
        output(") {\n");
        for (auto & i : statement->statements)
            printStatement(i);
        output("}\n");
    }

    static void printCaseLabelStatement(astCaseLabelStatement *statement) {
        if (statement->isDefault)
            output("default");
        else {
            output("case ");
            printExpression(statement->condition);
        }
        output(":\n");
    }

    static void printWhileStatement(astWhileStatement *statement) {
        output("while(");
        switch (statement->condition->type) {
            case astStatement::kDeclaration:
                printDeclarationStatement((astDeclarationStatement*)statement->condition, false);
                break;
            case astStatement::kExpression:
                printExpressionStatement((astExpressionStatement*)statement->condition, false);
                break;
            default:
                output("/* unexpected statement in while condition */\n");
                break;
        }
        output(")");
        printStatement(statement->body);
    }

    static void printDoStatement(astDoStatement *statement) {
        output("do");
        // deal with non compound (i.e scope) in do loops, e.g: do function_call(); while(expr);
        if (statement->body->type != astStatement::kCompound)
            output(" ");
        printStatement(statement->body);
        output("while(");
        printExpression(statement->condition);
        output(");\n");
    }

    static void printForStatement(astForStatement *statement) {
        output("for(");
        if (statement->init) {
            switch (statement->init->type) {
                case astStatement::kDeclaration:
                    printDeclarationStatement((astDeclarationStatement*)statement->init, kSemicolon);
                    break;
                case astStatement::kExpression:
                    printExpressionStatement((astExpressionStatement*)statement->init, kSemicolon);
                    break;
                default:
                    output("/* unexpected statement in while condition */\n");
                    break;
            }
        } else {
            output(";");
        }
        if (statement->condition) {
            output(" ");
            printExpression(statement->condition);
        }
        output(";");
        if (statement->loop) {
            output(" ");
            printExpression(statement->loop);
        }
        output(")");
        printStatement(statement->body);
    }

    static void printContinueStatement() {
        output("continue;\n");
    }

    static void printBreakStatement() {
        output("break;\n");
    }

    static void printReturnStatement(astReturnStatement *statement) {
        if (statement->expression) {
            output("return ");
            printExpression(statement->expression);
            output(";\n");
        } else {
            output("return;\n");
        }
    }

    static void printDiscardStatement() {
        output("discard;\n");
    }

    static void printDefine(astDefineStatement* define) {
        output("#define %s", define->name);

        if (!define->parameters.empty()) {
            output("(");
            for (size_t i = 0; i < define->parameters.size(); i++) {
                output("%s", define->parameters[i]);
                if (i != define->parameters.size() - 1) {
                    output(", ");
                }
            }
            output(")");
        }

        output(" ");

        if (define->value)
            printExpression(define->value);

        output("\n");
    }

    static void printIfDirective(astIfDirectiveStatement* directive) {
        output("#if ");
        printExpression(directive->value);
        output("\n");
        if (!directive->thenNodes.empty()) {
            printNodes(directive->thenNodes);
        }
        if (!directive->elseNodes.empty()) {
            printNodes (directive->elseNodes);
        }
    }

    static void printIfDefDirective(astIfDefDirectiveStatement* directive) {
        output("#ifdef ");
        printExpression(directive->value);
        output("\n");
        if (!directive->thenNodes.empty()) {
            printNodes(directive->thenNodes);
        }
        if (!directive->elseNodes.empty()) {
            printNodes (directive->elseNodes);
        }
    }

    static void printIfNDefDirective(astIfNDefDirectiveStatement* directive) {
        output("#ifndef ");
        printExpression(directive->value);
        output("\n");
        if (!directive->thenNodes.empty()) {
            printNodes(directive->thenNodes);
        }
        if (!directive->elseNodes.empty()) {
            printNodes (directive->elseNodes);
        }
    }

    static void printElseDirective(astElseDirectiveStatement* directive) {
        if (directive->value) {
            output("#elif ");
            printExpression(directive->value);
            output("\n");
        } else {
            output("#else\n");
        }
    }

    static void printEndIfDirective(astEndIfDirectiveStatement*) {
        output("#endif\n");
    }

    static void printStatement(astStatement *statement) {
        switch (statement->type) {
            case astStatement::kCompound:
                return printCompoundStatement((astCompoundStatement*)statement);
            case astStatement::kEmpty:
                return printEmptyStatement();
            case astStatement::kDeclaration:
                return printDeclarationStatement((astDeclarationStatement*)statement);
            case astStatement::kExpression:
                return printExpressionStatement((astExpressionStatement*)statement);
            case astStatement::kIf:
                return printIfStetement((astIfStatement*)statement);
            case astStatement::kSwitch:
                return printSwitchStatement((astSwitchStatement*)statement);
            case astStatement::kCaseLabel:
                return printCaseLabelStatement((astCaseLabelStatement*)statement);
            case astStatement::kWhile:
                return printWhileStatement((astWhileStatement*)statement);
            case astStatement::kDo:
                return printDoStatement((astDoStatement*)statement);
            case astStatement::kFor:
                return printForStatement((astForStatement*)statement);
            case astStatement::kContinue:
                return printContinueStatement();
            case astStatement::kBreak:
                return printBreakStatement();
            case astStatement::kReturn:
                return printReturnStatement((astReturnStatement*)statement);
            case astStatement::kDiscard:
                return printDiscardStatement();
            case astStatement::kDefine:
                return printDefine((astDefineStatement*) statement);
            case astStatement::kIfDirective:
                return printIfDirective((astIfDirectiveStatement*) statement);
            case astStatement::kIfDefDirective:
                return printIfDefDirective((astIfDefDirectiveStatement*) statement);
            case astStatement::kIfNDefDirective:
                return printIfNDefDirective((astIfNDefDirectiveStatement*) statement);
            case astStatement::kElseDirective:
                return printElseDirective((astElseDirectiveStatement*) statement);
            case astStatement::kEndIfDirective:
                return printEndIfDirective((astEndIfDirectiveStatement*) statement);
        }
        output("\n");
    }

    static void printFunctionParameter(astFunctionParameter *parameter) {
        printStorage(parameter->storage);
        printAuxiliary(parameter->auxiliary);
        printMemory(parameter->memory);
        printPrecision(parameter->precision);
        printType(parameter->baseType);
        if (parameter->name)
            output(" %s", parameter->name);
        if (parameter->isArray)
            printArraySize(parameter->arraySizes);
    }

    static void printFunction(astFunction *function) {
        printType(function->returnType);
        output(" %s(", function->name);
        for (size_t i = 0; i < function->parameters.size(); i++) {
            printFunctionParameter(function->parameters[i]);

            if (i != function->parameters.size() - 1) {
                output(", ");
            }
        }
        output(")");
        if (function->isPrototype) {
            output(";\n");
            return;
        }
        output(" {\n");
        for (auto & statement : function->statements)
            printStatement(statement);
        output("}\n");
    }

    static void printStructure(astStruct *structure) {
        output("struct ");
        if (structure->name)
            output("%s ", structure->name);
        output("{\n");
        for (auto & field : structure->fields) {
            printVariable(field);
            output(";\n");
        }
        output("};\n");
    }

    static void printInterfaceBlock(astInterfaceBlock *block) {
        printStorage(block->storage);
        output("%s ", block->name);
        output("{\n");
        for (auto & field : block->fields) {
            printVariable(field);
            output(";\n");
        }
        output("};\n");
    }

    static void printVersionDirective(astVersionDirective *version) {
        output("#version %d ", version->version);
        switch (version->type) {
            case kCore:
                output("core\n");
                break;
            case kCompatibility:
                output("compatibility\n");
                break;
            case kES:
                output("es\n");
                break;
        }
    }

    static void printExtensionDirective(astExtensionDirective *extension) {
        output("#extension %s : ", extension->name);
        switch (extension->behavior) {
            case kEnable:
                output("enable\n");
                break;
            case kRequire:
                output("require\n");
                break;
            case kWarn:
                output("warn\n");
                break;
            case kDisable:
                output("disable\n");
                break;
        }
    }

    void printNodes(const vector<astBase*>& nodes) {
        for (auto el : nodes) {
            switch (el->astType) {
                case astBase::kType: {
                    auto* type = (astType*) el;

                    if (type->typeType == astType::kBuiltin) {
                        output("// not printing a builtin type inside of a tu node\n");
                        continue;
                    }

                    if (type->typeType == astType::kStruct) {
                        printStructure((astStruct*) el);
                    } else if (type->typeType == astType::kInterfaceBlock) {
                        printInterfaceBlock((astInterfaceBlock*) el);
                    } else if (type->typeType == astType::kExtensionDirective) {
                        printExtensionDirective((astExtensionDirective*) el);
                    } else if (type->typeType == astType::kVersionDirective) {
                        printVersionDirective((astVersionDirective*) el);
                    }
                }
                    break;
                case astBase::kVariable: {
                    auto* variable = (astVariable*) el;

                    if (variable->type != astVariable::kGlobal) {
                        output("// not printing a non-global variable inside of a tu node\n");
                        continue;
                    }

                    printGlobalVariable((astGlobalVariable *) el);
                }
                    break;
                case astBase::kStatement: {
                    printStatement((astStatement *) el);
                }
                    break;
                case astBase::kFunction: {
                    printFunction((astFunction *) el);
                }
                    break;
                default:
                    output("/* unexpected node */\n");
                    break;
            }
        }
    }

    std::string printTU(astTU *tu) {
        printNodes(tu->nodes);

        std::string result(buffer);

        // reset buffer so it can be used again later
        buffer = "";

        return result;
    }
}