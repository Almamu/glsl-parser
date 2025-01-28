#pragma once

#include <memory.h>

#include "lexer.h"
#include "ast.h"

namespace glsl {

struct topLevel {
    topLevel()
        : storage(kStorageUnknown)
        , auxiliary(kAuxiliaryUnknown)
        , memory(0)
        , precision(-1)
        , interpolation(-1)
        , type(nullptr)
        , initialValue(nullptr)
        , arrayOnTypeOffset(0)
        , isInvariant(false)
        , isPrecise(false)
        , isArray(false)
        , name(nullptr)
    {
    }

    storageType storage;
    auxiliaryType auxiliary;
    int memory;
    int precision;
    int interpolation;
    astType *type;
    astConstantExpression *initialValue;
    vector<astConstantExpression*> arraySizes;
    size_t arrayOnTypeOffset;
    vector<astLayoutQualifier*> layoutQualifiers;
    bool isInvariant;
    bool isPrecise;
    bool isArray;
    char *name;
};

struct parserIncludeResolver {
    virtual const char* resolve(const char* name) = 0;
};

struct parser {
    ~parser();
    parser(const char *source, const char *fileName, parserIncludeResolver* includeResolvers, vector<const char*>* builtinVariables = nullptr);
    [[nodiscard]] astTU *parse(int type);

    [[nodiscard]] const char *error() const;

protected:
    enum {
        kEndConditionSemicolon = 1 << 0,
        kEndConditionParanthesis = 1 << 1,
        kEndConditionBracket = 1 << 2,
        kEndConditionColon = 1 << 3,
        kEndConditionComma = 1 << 4,
        kEndConditionLineFeed = 1 << 5,
    };

    typedef int endCondition;

    [[nodiscard]] bool next(bool ignore_eol = true, bool ignore_whitespace = true, bool eof_valid = false);

    [[nodiscard]] bool parseStorage(topLevel &current); // const, in, out, attribute, uniform, varying, buffer, shared
    [[nodiscard]] bool parseAuxiliary(topLevel &current); // centroid, sample, patch
    [[nodiscard]] bool parseInterpolation(topLevel &current); // smooth, flat, noperspective
    [[nodiscard]] bool parsePrecision(topLevel &current); // highp, mediump, lowp
    [[nodiscard]] bool parseInvariant(topLevel &current); // invariant
    [[nodiscard]] bool parsePrecise(topLevel &current); // precise
    [[nodiscard]] bool parseMemory(topLevel &current); // coherent, volatile, restrict, readonly, writeonly
    [[nodiscard]] bool parseLayout(topLevel &current);

    [[nodiscard]] bool parseTopLevelItem(topLevel &level, vector<astBase*>* nodes, topLevel *continuation = nullptr, bool allow_undefined = false);
    [[nodiscard]] bool parseTopLevel(vector<topLevel> &items, vector<astBase*>* nodes, bool allow_undefined = false);

    [[nodiscard]] bool isType(int type) const;
    [[nodiscard]] bool isKeyword(int keyword) const;
    [[nodiscard]] bool isOperator(int oper) const;
    [[nodiscard]] bool isEndCondition(endCondition condition) const;
    [[nodiscard]] bool isBuiltin() const;

    [[nodiscard]] static bool isConstantValue(astExpression *expression) ;
    [[nodiscard]] bool isConstant(astExpression *expression) const;

    void fatal(const char *fmt, ...);

    [[nodiscard]] astConstantExpression *evaluate(astExpression *expression);

    // Type parsers
    astBuiltin *parseBuiltin();
    astStruct *parseStruct();
    astInterfaceBlock *parseInterfaceBlock(int storage);

    [[nodiscard]] astFunction *parseFunction(const topLevel &parse);

    // Call parsers
    [[nodiscard]] astConstructorCall *parseConstructorCall(bool allow_undefined = false);
    [[nodiscard]] astFunctionCall *parseFunctionCall(bool allow_undefined = false);

    // Expression parsers
    [[nodiscard]] astExpression *parseExpression(endCondition condition, bool allow_undefined = false);
    [[nodiscard]] astExpression *parseUnary(endCondition condition, bool allow_undefined = false);
    [[nodiscard]] astExpression *parseBinary(int lhsPrecedence, astExpression *lhs, endCondition condition, bool allow_undefined = false);
    [[nodiscard]] astExpression *parseUnaryPrefix(endCondition condition, bool allow_undefined = false);
    [[nodiscard]] astConstantExpression *parseArraySize(bool allow_undefined = false);

    // Statement parsers
    [[nodiscard]] astStatement *parseStatement(bool allow_undefined = false);
    [[nodiscard]] astSwitchStatement *parseSwitchStatement();
    [[nodiscard]] astCaseLabelStatement *parseCaseLabelStatement();
    [[nodiscard]] astForStatement *parseForStatement();
    [[nodiscard]] astCompoundStatement *parseCompoundStatement();
    [[nodiscard]] astIfStatement *parseIfStatement();
    [[nodiscard]] astSimpleStatement *parseDeclarationOrExpressionStatement(endCondition condition, bool allow_undefined = false);
    [[nodiscard]] astDeclarationStatement *parseDeclarationStatement(endCondition condition, bool allow_undefined = false);
    [[nodiscard]] astExpressionStatement *parseExpressionStatement(endCondition condition, bool allow_undefined = false);
    [[nodiscard]] astContinueStatement *parseContinueStatement();
    [[nodiscard]] astBreakStatement *parseBreakStatement();
    [[nodiscard]] astDiscardStatement *parseDiscardStatement();
    [[nodiscard]] astReturnStatement *parseReturnStatement();
    [[nodiscard]] astDoStatement *parseDoStatement();
    [[nodiscard]] astWhileStatement *parseWhileStatement();

    [[nodiscard]] astDefineStatement* parseDefineDirective();
    [[nodiscard]] bool parseIfDirectiveVariations(astIfDirectiveStatement* out, bool is_in_root);
    [[nodiscard]] astIfDefDirectiveStatement* parseIfDefDirective(bool is_in_root = true);
    [[nodiscard]] astIfNDefDirectiveStatement* parseIfNDefDirective(bool is_in_root = true);
    [[nodiscard]] astIfDirectiveStatement* parseIfDirective(bool is_in_root = true);
    [[nodiscard]] bool parseIncludeDirective();
    [[nodiscard]] astStatement *parseDirective();

    astBinaryExpression *createExpression();

    astType *findType(const char *identifier);
    astVariable *findVariable(const char *identifier);
    astDefineStatement *findDefine(const char *define);
    astType* getType(astExpression *expression);
private:
    typedef vector<astVariable *> scope;
    typedef vector<astDefineStatement*> defineScope;

    // Specialized in .cpp
    template<typename T>
    [[nodiscard]] T *parseBlock(const char* type);

    astTU *m_ast;
    lexer m_lexer;
    token m_token;
    vector<scope> m_scopes;
    vector<defineScope> m_defines;
    vector<astBuiltin*> m_builtins;
    char *m_error;
    char *m_oom;
    const char *m_fileName;
    bool m_mainFound;

    static void strdel(char **what) {
        if (!*what)
            return;
        free(*what);
        *what = nullptr;
    }

    char *strnew(const char *what) {
        if (!what)
            return nullptr;
        size_t length = strlen(what) + 1;
        char *copy = (char*)malloc(length);
        memcpy(copy, what, length);
        m_strings.push_back(copy);
        return copy;
    }

    static bool strnil(const char *what) {
        return !what || !*what;
    }

    vector<astMemory> m_memory; // Memory of AST held here
    vector<char *> m_strings; // Memory of strings held here
    vector<const char*>* m_builtinVariables;
    parserIncludeResolver* m_includeResolver;
};

}
