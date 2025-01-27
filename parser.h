#ifndef PARSE_HDR
#define PARSE_HDR
#include "lexer.h"
#include "ast.h"

namespace glsl {

#if __GNUC__ >= 4
#   define CHECK_RETURN __attribute__((warn_unused_result))
#elif _MSC_VER >= 1700
#   define CHECK_RETURN _Check_return_
#else
#   define CHECK_RETURN
#endif

struct topLevel {
    topLevel()
        : storage(kStorageUnknown)
        , auxiliary(kAuxiliaryUnknown)
        , memory(0)
        , precision(-1)
        , interpolation(-1)
        , type(0)
        , initialValue(0)
        , arrayOnTypeOffset(0)
        , isInvariant(false)
        , isPrecise(false)
        , isArray(false)
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

struct parser {
    ~parser();
    parser(const char *source, const char *fileName);
    CHECK_RETURN astTU *parse(int type, vector<const char*>* builtinVariables = 0);

    const char *error() const;

protected:
    void cleanup();

    enum {
        kEndConditionSemicolon = 1 << 0,
        kEndConditionParanthesis = 1 << 1,
        kEndConditionBracket = 1 << 2,
        kEndConditionColon = 1 << 3,
        kEndConditionComma = 1 << 4,
        kEndConditionLineFeed = 1 << 5,
        kEndConditionElseEndIfDirective = 1 << 6,
    };

    typedef int endCondition;

    CHECK_RETURN bool next(bool ignore_eol = true, bool ignore_whitespace = true, bool eof_valid = false);

    CHECK_RETURN bool parseStorage(topLevel &current); // const, in, out, attribute, uniform, varying, buffer, shared
    CHECK_RETURN bool parseAuxiliary(topLevel &current); // centroid, sample, patch
    CHECK_RETURN bool parseInterpolation(topLevel &current); // smooth, flat, noperspective
    CHECK_RETURN bool parsePrecision(topLevel &current); // highp, mediump, lowp
    CHECK_RETURN bool parseInvariant(topLevel &current); // invariant
    CHECK_RETURN bool parsePrecise(topLevel &current); // precise
    CHECK_RETURN bool parseMemory(topLevel &current); // coherent, volatile, restrict, readonly, writeonly
    CHECK_RETURN bool parseLayout(topLevel &current);

    CHECK_RETURN bool parseTopLevelItem(topLevel &level, vector<astBase*>* nodes, topLevel *continuation = 0, bool allow_undefined = false);
    CHECK_RETURN bool parseTopLevel(vector<topLevel> &top, vector<astBase*>* nodes, bool allow_undefined = false);

    CHECK_RETURN bool isType(int type) const;
    CHECK_RETURN bool isKeyword(int keyword) const;
    CHECK_RETURN bool isOperator(int oper) const;
    CHECK_RETURN bool isEndCondition(endCondition condition) const;
    CHECK_RETURN bool isBuiltin() const;

    CHECK_RETURN bool isConstantValue(astExpression *expression) const;
    CHECK_RETURN bool isConstant(astExpression *expression) const;

    void fatal(const char *fmt, ...);

    CHECK_RETURN astConstantExpression *evaluate(astExpression *expression);

    // Type parsers
    astBuiltin *parseBuiltin();
    astStruct *parseStruct();
    astInterfaceBlock *parseInterfaceBlock(int storage);

    CHECK_RETURN astFunction *parseFunction(const topLevel &parse);

    // Call parsers
    CHECK_RETURN astConstructorCall *parseConstructorCall(bool allow_undefined = false);
    CHECK_RETURN astFunctionCall *parseFunctionCall(bool allow_undefined = false);

    // Expression parsers
    CHECK_RETURN astExpression *parseExpression(endCondition end, bool allow_undefined = false);
    CHECK_RETURN astExpression *parseUnary(endCondition end, bool allow_undefined = false);
    CHECK_RETURN astExpression *parseBinary(int lhsPrecedence, astExpression *lhs, endCondition condition, bool allow_undefined = false);
    CHECK_RETURN astExpression *parseUnaryPrefix(endCondition end, bool allow_undefined = false);
    CHECK_RETURN astConstantExpression *parseArraySize(bool allow_undefined = false);

    // Statement parsers
    CHECK_RETURN astStatement *parseStatement(bool allow_undefined = false);
    CHECK_RETURN astSwitchStatement *parseSwitchStatement();
    CHECK_RETURN astCaseLabelStatement *parseCaseLabelStatement();
    CHECK_RETURN astForStatement *parseForStatement();
    CHECK_RETURN astCompoundStatement *parseCompoundStatement();
    CHECK_RETURN astIfStatement *parseIfStatement();
    CHECK_RETURN astSimpleStatement *parseDeclarationOrExpressionStatement(endCondition condition, bool allow_undefined = false);
    CHECK_RETURN astDeclarationStatement *parseDeclarationStatement(endCondition condition, bool allow_undefined = false);
    CHECK_RETURN astExpressionStatement *parseExpressionStatement(endCondition condition, bool allow_undefined = false);
    CHECK_RETURN astContinueStatement *parseContinueStatement();
    CHECK_RETURN astBreakStatement *parseBreakStatement();
    CHECK_RETURN astDiscardStatement *parseDiscardStatement();
    CHECK_RETURN astReturnStatement *parseReturnStatement();
    CHECK_RETURN astDoStatement *parseDoStatement();
    CHECK_RETURN astWhileStatement *parseWhileStatement();

    CHECK_RETURN astDefineStatement* parseDefineDirective();
    CHECK_RETURN bool parseIfDirectiveVariations(astIfDirectiveStatement* out, bool is_in_root);
    CHECK_RETURN astIfDefDirectiveStatement* parseIfDefDirective(bool is_in_root = true);
    CHECK_RETURN astIfNDefDirectiveStatement* parseIfNDefDirective(bool is_in_root = true);
    CHECK_RETURN astIfDirectiveStatement* parseIfDirective(bool is_in_root = true);
    CHECK_RETURN astStatement *parseDirective();

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
    CHECK_RETURN T *parseBlock(const char* type);

    astTU *m_ast;
    lexer m_lexer;
    token m_token;
    vector<scope> m_scopes;
    vector<defineScope> m_defines;
    vector<astBuiltin*> m_builtins;
    char *m_error;
    char *m_oom;
    const char *m_fileName;

    void strdel(char **what) {
        if (!*what)
            return;
        free(*what);
        *what = 0;
    }

    char *strnew(const char *what) {
        if (!what)
            return 0;
        size_t length = strlen(what) + 1;
        char *copy = (char*)malloc(length);
        memcpy(copy, what, length);
        m_strings.push_back(copy);
        return copy;
    }

    bool strnil(const char *what) {
        return !what || !*what;
    }

    vector<astMemory> m_memory; // Memory of AST held here
    vector<char *> m_strings; // Memory of strings held here
};

}

#endif
