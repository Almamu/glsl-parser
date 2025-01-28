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

    struct printerContext {
        std::string buffer;
        int indentLevel;
    };

    static void output(printerContext& context, const char* format, ...) {
        va_list args;
        va_start(args, format);

        int size = vsnprintf(nullptr, 0, format, args);

        if (size < 0) {
            va_end(args);
            return;
        }

        size_t oldSize = context.buffer.size();
        context.buffer.resize(oldSize + size);

        va_start (args, format); // restart args
        vsnprintf(&context.buffer[oldSize], size + 1, format, args);
        va_end(args);
    }

    static void indent(printerContext& context) {
        for (int i = 0; i < context.indentLevel; i++)
            output(context, "\t");
    }

    static bool isAfterMultipleLinefeeds(printerContext& context) {
        return context.buffer.size() > 2 && context.buffer[context.buffer.size() - 2] == '\n' && context.buffer[context.buffer.size() - 1] == '\n';
    }

    static void printExpression(printerContext& context, astExpression *expression);
    static void printStatement(printerContext& context, astStatement *statement, bool should_indent = true);
    static void printNodes(printerContext& context, const vector<astBase*>& nodes);

    static void printBuiltin(printerContext& context, astBuiltin *builtin) {
        output(context, "%s", kTypes[builtin->type]);
    }

    static void printType(printerContext& context, astType *type) {
        if (type->typeType == astType::kBuiltin)
            printBuiltin(context, (astBuiltin*)type);
        else
            output(context, "%s", ((astStruct *) type)->name);
    }

    static void printIntConstant(printerContext& context, astIntConstant *expression) {
        output(context, "%d", expression->value);
    }

    static void printUIntConstant(printerContext& context, astUIntConstant *expression) {
        output(context, "%du", expression->value);
    }

    static void printFloatConstant(printerContext& context, astFloatConstant *expression) {
        char format[1024];
        snprintf(format, sizeof format, "%g", expression->value);
        if (!strchr(format, '.') && !strchr(format, 'e'))
            output(context, "%g.0", expression->value);
        else
            output(context, "%s", format);
    }

    static void printDoubleConstant(printerContext& context, astDoubleConstant *expression) {
        output(context, "%g", expression->value);
    }

    static void printBoolConstant(printerContext& context, astBoolConstant *expression) {
        output(context, "%s", expression->value ? "true" : "false");
    }

    static void printArraySize(printerContext& context, const vector<astConstantExpression*> &arraySizes) {
        for (auto arraySize : arraySizes) {
            output(context, "[");
            printExpression(context, arraySize);
            output(context, "]");
        }
    }

    static void printVariable(printerContext& context, astVariable *variable, bool nameOnly = false) {
        if (variable->isPrecise)
            output(context, "precise ");

        if (nameOnly) {
            output(context, "%s", variable->name);
            return;
        }

        printType(context, variable->baseType);
        output(context, " %s", variable->name);

        if (nameOnly)
            return;

        if (variable->isArray)
            printArraySize(context, variable->arraySizes);
    }

    static void printStorage(printerContext& context, int storage) {
        switch (storage) {
            case kConst:
                output(context, "const ");
                break;
            case kIn:
                output(context, "in ");
                break;
            case kOut:
                output(context, "out ");
                break;
            case kAttribute:
                output(context, "attribute ");
                break;
            case kUniform:
                output(context, "uniform ");
                break;
            case kVarying:
                output(context, "varying ");
                break;
            case kBuffer:
                output(context, "buffer ");
                break;
            case kShared:
                output(context, "shared ");
                break;
            default:
                break;
        }
    }

    static void printAuxiliary(printerContext& context, int auxiliary) {
        switch (auxiliary) {
            case kCentroid:
                output(context, "centroid ");
                break;
            case kSample:
                output(context, "sample ");
                break;
            case kPatch:
                output(context, "patch ");
                break;
            default:
                break;
        }
    }

    static void printMemory(printerContext& context, int memory) {
        if (memory & kCoherent) output(context, "coherent ");
        if (memory & kVolatile) output(context, "volatile ");
        if (memory & kRestrict) output(context, "restrict ");
        if (memory & kReadOnly) output(context, "readonly ");
        if (memory & kWriteOnly) output(context, "writeonly ");
    }

    static void printPrecision(printerContext& context, int precision) {
        switch (precision) {
            case kLowp:
                output(context, "lowp ");
                break;
            case kMediump:
                output(context, "mediump ");
                break;
            case kHighp:
                output(context, "highp ");
                break;
            default:
                break;
        }
    }

    static void printGlobalVariable(printerContext& context, astGlobalVariable *variable) {
        indent(context);
        vector<astLayoutQualifier*> &qualifiers = variable->layoutQualifiers;
        if (!variable->layoutQualifiers.empty()) {
            output(context, "layout (");
            for (size_t i = 0; i < qualifiers.size(); i++) {
                astLayoutQualifier *qualifier = qualifiers[i];
                output(context, "%s", qualifier->name);
                if (qualifier->initialValue) {
                    output(context, " = ");
                    printExpression(context, qualifier->initialValue);
                }
                if (i != qualifiers.size() - 1)
                    output(context, ", ");
            }
            output(context, ") ");
        }

        printStorage(context, variable->storage);
        printAuxiliary(context, variable->auxiliary);
        printMemory(context, variable->memory);
        printPrecision(context, variable->precision);

        if (variable->isInvariant)
            output(context, "invariant ");

        switch (variable->interpolation) {
            case kSmooth:
                output(context, "smooth ");
                break;
            case kFlat:
                output(context, "flat ");
                break;
            case kNoPerspective:
                output(context, "noperspective ");
                break;
        }

        printVariable(context, (astVariable*)variable);

        if (variable->initialValue) {
            output(context, " = ");
            printExpression(context, variable->initialValue);
        }

        output(context, ";\n");
    }

    static void printVariableIdentifier(printerContext& context, astVariableIdentifier *expression) {
        printVariable(context, expression->variable, true);
    }

    static void printDefineIdentifier(printerContext& context, astDefineIdentifier *expression) {
        output(context, "%s", expression->define->name);
    }

    static void printUnknownIdentifier(printerContext& context, astUnknownIdentifier *expression) {
        output(context, "%s", expression->define);
    }

    static void printFieldOrSwizzle(printerContext& context, astFieldOrSwizzle *expression) {
        printExpression(context, expression->operand);
        output(context, ".%s", expression->name);
    }

    static void printArraySubscript(printerContext& context, astArraySubscript *expression) {
        printExpression(context, expression->operand);
        output(context, "[");
        printExpression(context, expression->index);
        output(context, "]");
    }

    static void printFunctionCall(printerContext& context, astFunctionCall *expression) {
        output(context, "%s(", expression->name);
        for (size_t i = 0; i < expression->parameters.size(); i++) {
            printExpression(context, expression->parameters[i]);
            if (i != expression->parameters.size() - 1)
                output(context, ", ");
        }
        output(context, ")");
    }

    static void printConstructorCall(printerContext& context, astConstructorCall *expression) {
        printType(context, expression->type);
        output(context, "(");
        for (size_t i = 0; i < expression->parameters.size(); i++) {
            printExpression(context, expression->parameters[i]);
            if (i != expression->parameters.size() - 1)
                output(context, ", ");
        }
        output(context, ")");
    }

    enum { kSemicolon = 1 << 0, kNewLine = 1 << 1, kDefault = kSemicolon | kNewLine };

    static void printFunctionVariable(printerContext& context, astFunctionVariable *variable, int flags = kDefault ) {
        if (variable->isConst)
            output(context, "const ");
        printVariable(context, (astVariable*)variable);
        if (variable->initialValue) {
            output(context, " = ");
            printExpression(context, variable->initialValue);
        }
        if (flags & kSemicolon) output(context, ";");
        if (flags & kNewLine) output(context, "\n");
    }

    static void printPostIncrement(printerContext& context, astPostIncrementExpression *expression) {
        printExpression(context, expression->operand);
        output(context, "++");
    }

    static void printPostDecrement(printerContext& context, astPostDecrementExpression *expression) {
        printExpression(context, expression->operand);
        output(context, "--");
    }

    static void printUnaryMinus(printerContext& context, astUnaryMinusExpression *expression) {
        output(context, "-");
        printExpression(context, expression->operand);
    }

    static void printUnaryPlus(printerContext& context, astUnaryPlusExpression *expression) {
        output(context, "+");
        printExpression(context, expression->operand);
    }

    static void printUnaryBitNot(printerContext& context, astUnaryBitNotExpression *expression) {
        output(context, "~");
        printExpression(context, expression->operand);
    }

    static void printUnaryLogicalNot(printerContext& context, astUnaryLogicalNotExpression *expression) {
        output(context, "!");
        printExpression(context, expression->operand);
    }

    static void printPrefixIncrement(printerContext& context, astPrefixIncrementExpression *expression) {
        output(context, "++");
        printExpression(context, expression->operand);
    }

    static void printPrefixDecrement(printerContext& context, astPrefixDecrementExpression *expression) {
        output(context, "--");
        printExpression(context, expression->operand);
    }

    static void printAssign(printerContext& context, astAssignmentExpression *expression) {
        printExpression(context, expression->operand1);
        output(context, " %s ", kOperators[expression->assignment]);
        printExpression(context, expression->operand2);
    }

    static void printSequence(printerContext& context, astSequenceExpression *expression) {
        output(context, "(");
        printExpression(context, expression->operand1);
        output(context, ", ");
        printExpression(context, expression->operand2);
        output(context, ")");
    }

    static void printOperation(printerContext& context, astOperationExpression *expression) {
        output(context, "(");
        printExpression(context, expression->operand1);
        output(context, " %s ", kOperators[expression->operation]);
        printExpression(context, expression->operand2);
        output(context, ")");
    }

    static void printTernary(printerContext& context, astTernaryExpression *expression) {
        output(context, "(");
        printExpression(context, expression->condition);
        output(context, " ? ");
        printExpression(context, expression->onTrue);
        output(context, " : ");
        printExpression(context, expression->onFalse);
        output(context, ")");
    }

    static void printExpression(printerContext& context, astExpression *expression) {
        switch (expression->type) {
            case astExpression::kIntConstant:
                return printIntConstant(context, (astIntConstant*)expression);
            case astExpression::kUIntConstant:
                return printUIntConstant(context, (astUIntConstant*)expression);
            case astExpression::kFloatConstant:
                return printFloatConstant(context, (astFloatConstant*)expression);
            case astExpression::kDoubleConstant:
                return printDoubleConstant(context, (astDoubleConstant*)expression);
            case astExpression::kBoolConstant:
                return printBoolConstant(context, (astBoolConstant*)expression);
            case astExpression::kVariableIdentifier:
                return printVariableIdentifier(context, (astVariableIdentifier*)expression);
            case astExpression::kDefineIdentifier:
                return printDefineIdentifier(context, (astDefineIdentifier*)expression);
            case astExpression::kUnknownIdentifier:
                return printUnknownIdentifier(context, (astUnknownIdentifier*)expression);
            case astExpression::kFieldOrSwizzle:
                return printFieldOrSwizzle(context, (astFieldOrSwizzle*)expression);
            case astExpression::kArraySubscript:
                return printArraySubscript(context, (astArraySubscript*)expression);
            case astExpression::kFunctionCall:
                return printFunctionCall(context, (astFunctionCall*)expression);
            case astExpression::kConstructorCall:
                return printConstructorCall(context, (astConstructorCall*)expression);
            case astExpression::kPostIncrement:
                return printPostIncrement(context, (astPostIncrementExpression*)expression);
            case astExpression::kPostDecrement:
                return printPostDecrement(context, (astPostDecrementExpression*)expression);
            case astExpression::kUnaryMinus:
                return printUnaryMinus(context, (astUnaryMinusExpression*)expression);
            case astExpression::kUnaryPlus:
                return printUnaryPlus(context, (astUnaryPlusExpression*)expression);
            case astExpression::kBitNot:
                return printUnaryBitNot(context, (astUnaryBitNotExpression*)expression);
            case astExpression::kLogicalNot:
                return printUnaryLogicalNot(context, (astUnaryLogicalNotExpression*)expression);
            case astExpression::kPrefixIncrement:
                return printPrefixIncrement(context, (astPrefixIncrementExpression*)expression);
            case astExpression::kPrefixDecrement:
                return printPrefixDecrement(context, (astPrefixDecrementExpression*)expression);
            case astExpression::kAssign:
                return printAssign(context, (astAssignmentExpression*)expression);
            case astExpression::kSequence:
                return printSequence(context, (astSequenceExpression*)expression);
            case astExpression::kOperation:
                return printOperation(context, (astOperationExpression*)expression);
            case astExpression::kTernary:
                return printTernary(context, (astTernaryExpression*)expression);
        }
    }

    static void printCompoundStatement(printerContext& context, astCompoundStatement *statement) {
        output(context, "{\n");
        context.indentLevel ++;
        for (auto & i : statement->statements)
            printStatement(context, i);
        context.indentLevel --;
        indent(context);
        output(context, "}\n");
    }

    static void printEmptyStatement(printerContext& context) {
        output(context, ";");
    }

    static void printDeclarationStatement(printerContext& context, astDeclarationStatement *statement, int flags = kDefault) {
        for (auto & variable : statement->variables)
            printFunctionVariable(context, variable, flags);
    }

    static void printExpressionStatement(printerContext& context, astExpressionStatement *statement, int flags = kDefault) {
        printExpression(context, statement->expression);
        if (flags & kSemicolon) output(context, ";");
        if (flags & kNewLine) output(context, "\n");
    }

    static void printIfStetement(printerContext& context, astIfStatement *statement) {
        output(context, "if(");
        printExpression(context, statement->condition);
        output(context, ") ");
        printStatement(context, statement->thenStatement, false);
        if (statement->elseStatement) {
            indent(context);
            output(context, "else ");
            printStatement(context, statement->elseStatement, false);
        }
    }

    static void printSwitchStatement(printerContext& context, astSwitchStatement *statement) {
        output(context, "switch(");
        printExpression(context, statement->expression);
        output(context, ") {\n");
        for (auto & i : statement->statements)
            printStatement(context, i);
        output(context, "}\n");
    }

    static void printCaseLabelStatement(printerContext& context, astCaseLabelStatement *statement) {
        if (statement->isDefault)
            output(context, "default");
        else {
            output(context, "case ");
            printExpression(context, statement->condition);
        }
        output(context, ":\n");
    }

    static void printWhileStatement(printerContext& context, astWhileStatement *statement) {
        output(context, "while(");
        switch (statement->condition->type) {
            case astStatement::kDeclaration:
                printDeclarationStatement(context, (astDeclarationStatement*)statement->condition, false);
                break;
            case astStatement::kExpression:
                printExpressionStatement(context, (astExpressionStatement*)statement->condition, false);
                break;
            default:
                output(context, "/* unexpected statement in while condition */\n");
                break;
        }
        output(context, ") ");
        printStatement(context, statement->body, false);
    }

    static void printDoStatement(printerContext& context, astDoStatement *statement) {
        output(context, "do ");
        printStatement(context, statement->body, false);
        output(context, "while(");
        printExpression(context, statement->condition);
        output(context, ");\n");
    }

    static void printForStatement(printerContext& context, astForStatement *statement) {
        output(context, "for(");
        if (statement->init) {
            switch (statement->init->type) {
                case astStatement::kDeclaration:
                    printDeclarationStatement(context, (astDeclarationStatement*)statement->init, kSemicolon);
                    break;
                case astStatement::kExpression:
                    printExpressionStatement(context, (astExpressionStatement*)statement->init, kSemicolon);
                    break;
                default:
                    output(context, "/* unexpected statement in while condition */\n");
                    break;
            }
        } else {
            output(context, ";");
        }
        if (statement->condition) {
            output(context, " ");
            printExpression(context, statement->condition);
        }
        output(context, ";");
        if (statement->loop) {
            output(context, " ");
            printExpression(context, statement->loop);
        }
        output(context, ") ");
        printStatement(context, statement->body, false);
    }

    static void printContinueStatement(printerContext& context) {
        output(context, "continue;\n");
    }

    static void printBreakStatement(printerContext& context) {
        output(context, "break;\n");
    }

    static void printReturnStatement(printerContext& context, astReturnStatement *statement) {
        if (statement->expression) {
            output(context, "return ");
            printExpression(context, statement->expression);
            output(context, ";\n");
        } else {
            output(context, "return;\n");
        }
    }

    static void printDiscardStatement(printerContext& context) {
        output(context, "discard;\n");
    }

    static void printDefine(printerContext& context, astDefineStatement* define) {
        output(context, "#define %s", define->name);

        if (!define->parameters.empty()) {
            output(context, "(");
            for (size_t i = 0; i < define->parameters.size(); i++) {
                output(context, "%s", define->parameters[i]);
                if (i != define->parameters.size() - 1) {
                    output(context, ", ");
                }
            }
            output(context, ")");
        }

        output(context, " ");

        if (define->value)
            printExpression(context, define->value);

        output(context, "\n");
    }

    static void printIfDirective(printerContext& context, astIfDirectiveStatement* directive) {
        // take out identation to check for multiline
        if (context.indentLevel > 0) {
            for (int i = 0; i < context.indentLevel; i++)
                context.buffer.pop_back();
        }

        if (isAfterMultipleLinefeeds(context)) {
            context.buffer.pop_back();
        }

        output(context, "\n");

        // add indentation back
        indent(context);

        output(context, "#if ");
        printExpression(context, directive->value);
        output(context, "\n");
        if (!directive->thenNodes.empty()) {
            context.indentLevel ++;
            printNodes(context, directive->thenNodes);
            context.indentLevel --;
        }
        if (!directive->elseNodes.empty()) {
            context.indentLevel ++;
            printNodes (context, directive->elseNodes);
            context.indentLevel --;
        }
    }

    static void printIfDefDirective(printerContext& context, astIfDefDirectiveStatement* directive) {
        // take out identation to check for multiline
        if (context.indentLevel > 0) {
            for (int i = 0; i < context.indentLevel; i++)
                context.buffer.pop_back();
        }

        if (isAfterMultipleLinefeeds(context)) {
            context.buffer.pop_back();
        }

        output(context, "\n");

        // add indentation back
        indent(context);

        output(context, "#ifdef ");
        printExpression(context, directive->value);
        output(context, "\n");
        if (!directive->thenNodes.empty()) {
            context.indentLevel ++;
            printNodes(context, directive->thenNodes);
            context.indentLevel --;
        }
        if (!directive->elseNodes.empty()) {
            context.indentLevel ++;
            printNodes (context, directive->elseNodes);
            context.indentLevel --;
        }
    }

    static void printIfNDefDirective(printerContext& context, astIfNDefDirectiveStatement* directive) {
        // take out identation to check for multiline
        if (context.indentLevel > 0) {
            for (int i = 0; i < context.indentLevel; i++)
                context.buffer.pop_back();
        }

        if (isAfterMultipleLinefeeds(context)) {
            context.buffer.pop_back();
        }

        output(context, "\n");

        // add indentation back
        indent(context);

        output(context, "#ifndef ");
        printExpression(context, directive->value);
        output(context, "\n");
        if (!directive->thenNodes.empty()) {
            context.indentLevel ++;
            printNodes(context, directive->thenNodes);
            context.indentLevel --;
        }
        if (!directive->elseNodes.empty()) {
            context.indentLevel ++;
            printNodes (context, directive->elseNodes);
            context.indentLevel --;
        }
    }

    static void printElseDirective(printerContext& context, astElseDirectiveStatement* directive) {
        // take out identation to check for multiline
        if (context.indentLevel > 0) {
            for (int i = 0; i < context.indentLevel; i++)
                context.buffer.pop_back();
        }

        if (isAfterMultipleLinefeeds(context)) {
            context.buffer.pop_back();
        }

        // add indentation back
        indent(context);

        // XXXHACK: REMOVE ONE LEVEL OF IDENTATION IF PRESENT
        if (context.indentLevel > 0) {
            context.buffer.pop_back();
        }

        if (directive->value) {
            output(context, "#elif ");
            printExpression(context, directive->value);
            output(context, "\n");
        } else {
            output(context, "#else\n");
        }
    }

    static void printEndIfDirective(printerContext& context, astEndIfDirectiveStatement*) {
        // take out identation to check for multiline
        if (context.indentLevel > 0) {
            for (int i = 0; i < context.indentLevel; i++)
                context.buffer.pop_back();
        }

        if (isAfterMultipleLinefeeds(context)) {
            context.buffer.pop_back();
        }

        // add indentation back
        indent(context);

        // XXXHACK: REMOVE ONE LEVEL OF IDENTATION IF PRESENT
        if (context.indentLevel > 0) {
            context.buffer.pop_back();
        }
        output(context, "#endif\n\n");
    }

    static void printStatement(printerContext& context, astStatement *statement, bool should_indent) {
        if (should_indent) {
            indent(context);
        }

        switch (statement->type) {
            case astStatement::kCompound:
                return printCompoundStatement(context, (astCompoundStatement*)statement);
            case astStatement::kEmpty:
                return printEmptyStatement(context);
            case astStatement::kDeclaration:
                return printDeclarationStatement(context, (astDeclarationStatement*)statement);
            case astStatement::kExpression:
                return printExpressionStatement(context, (astExpressionStatement*)statement);
            case astStatement::kIf:
                return printIfStetement(context, (astIfStatement*)statement);
            case astStatement::kSwitch:
                return printSwitchStatement(context, (astSwitchStatement*)statement);
            case astStatement::kCaseLabel:
                return printCaseLabelStatement(context, (astCaseLabelStatement*)statement);
            case astStatement::kWhile:
                return printWhileStatement(context, (astWhileStatement*)statement);
            case astStatement::kDo:
                return printDoStatement(context, (astDoStatement*)statement);
            case astStatement::kFor:
                return printForStatement(context, (astForStatement*)statement);
            case astStatement::kContinue:
                return printContinueStatement(context);
            case astStatement::kBreak:
                return printBreakStatement(context);
            case astStatement::kReturn:
                return printReturnStatement(context, (astReturnStatement*)statement);
            case astStatement::kDiscard:
                return printDiscardStatement(context);
            case astStatement::kDefine:
                return printDefine(context, (astDefineStatement*) statement);
            case astStatement::kIfDirective:
                return printIfDirective(context, (astIfDirectiveStatement*) statement);
            case astStatement::kIfDefDirective:
                return printIfDefDirective(context, (astIfDefDirectiveStatement*) statement);
            case astStatement::kIfNDefDirective:
                return printIfNDefDirective(context, (astIfNDefDirectiveStatement*) statement);
            case astStatement::kElseDirective:
                return printElseDirective(context, (astElseDirectiveStatement*) statement);
            case astStatement::kEndIfDirective:
                return printEndIfDirective(context, (astEndIfDirectiveStatement*) statement);
        }
        output(context, "\n");
    }

    static void printFunctionParameter(printerContext& context, astFunctionParameter *parameter) {
        printStorage(context, parameter->storage);
        printAuxiliary(context, parameter->auxiliary);
        printMemory(context, parameter->memory);
        printPrecision(context, parameter->precision);
        printType(context, parameter->baseType);
        if (parameter->name)
            output(context, " %s", parameter->name);
        if (parameter->isArray)
            printArraySize(context, parameter->arraySizes);
    }

    static void printFunction(printerContext& context, astFunction *function) {
        // some extra spacing before and after is always welcomed
        if (!function->isPrototype && isAfterMultipleLinefeeds(context)) {
            output(context, "\n");
        }
        indent(context);
        printType(context, function->returnType);
        output(context, " %s(", function->name);
        for (size_t i = 0; i < function->parameters.size(); i++) {
            printFunctionParameter(context, function->parameters[i]);

            if (i != function->parameters.size() - 1) {
                output(context, ", ");
            }
        }
        output(context, ")");
        if (function->isPrototype) {
            output(context, ";\n");
            return;
        }
        output(context, " {\n");
        context.indentLevel ++;
        for (auto & statement : function->statements)
            printStatement(context, statement);
        context.indentLevel --;
        indent(context);
        output(context, "}\n\n");
    }

    static void printStructure(printerContext& context, astStruct *structure) {
        indent(context);
        output(context, "struct ");
        if (structure->name)
            output(context, "%s ", structure->name);
        output(context, "{\n");
        context.indentLevel ++;
        for (auto & field : structure->fields) {
            indent(context);
            printVariable(context, field);
            output(context, ";\n");
        }
        context.indentLevel --;
        output(context, "};\n");
    }

    static void printInterfaceBlock(printerContext& context, astInterfaceBlock *block) {
        indent(context);
        printStorage(context, block->storage);
        output(context, "%s ", block->name);
        output(context, "{\n");
        context.indentLevel ++;
        for (auto & field : block->fields) {
            indent(context);
            printVariable(context, field);
            output(context, ";\n");
        }
        context.indentLevel --;
        output(context, "};\n");
    }

    static void printVersionDirective(printerContext& context, astVersionDirective *version) {
        indent(context);
        output(context, "#version %d ", version->version);
        switch (version->type) {
            case kCore:
                output(context, "core\n");
                break;
            case kCompatibility:
                output(context, "compatibility\n");
                break;
            case kES:
                output(context, "es\n");
                break;
        }
    }

    static void printExtensionDirective(printerContext& context, astExtensionDirective *extension) {
        indent(context);
        output(context, "#extension %s : ", extension->name);
        switch (extension->behavior) {
            case kEnable:
                output(context, "enable\n");
                break;
            case kRequire:
                output(context, "require\n");
                break;
            case kWarn:
                output(context, "warn\n");
                break;
            case kDisable:
                output(context, "disable\n");
                break;
        }
    }

    void printNodes(printerContext& context, const vector<astBase*>& nodes) {
        for (auto el : nodes) {
            switch (el->astType) {
                case astBase::kType: {
                    auto* type = (astType*) el;

                    if (type->typeType == astType::kBuiltin) {
                        output(context, "// not printing a builtin type inside of a tu node\n");
                        continue;
                    }

                    if (type->typeType == astType::kStruct) {
                        printStructure(context, (astStruct*) el);
                    } else if (type->typeType == astType::kInterfaceBlock) {
                        printInterfaceBlock(context, (astInterfaceBlock*) el);
                    } else if (type->typeType == astType::kExtensionDirective) {
                        printExtensionDirective(context, (astExtensionDirective*) el);
                    } else if (type->typeType == astType::kVersionDirective) {
                        printVersionDirective(context, (astVersionDirective*) el);
                    }
                }
                    break;
                case astBase::kVariable: {
                    auto* variable = (astVariable*) el;

                    if (variable->type != astVariable::kGlobal) {
                        output(context, "// not printing a non-global variable inside of a tu node\n");
                        continue;
                    }

                    printGlobalVariable(context, (astGlobalVariable *) el);
                }
                    break;
                case astBase::kStatement: {
                    printStatement(context, (astStatement *) el);
                }
                    break;
                case astBase::kFunction: {
                    printFunction(context, (astFunction *) el);
                }
                    break;
                default:
                    output(context, "/* unexpected node */\n");
                    break;
            }
        }
    }

    std::string printTU(astTU *tu) {
        printerContext context = {
            .buffer = "",
            .indentLevel = 0
        };

        printNodes(context, tu->nodes);

        return context.buffer;
    }
}