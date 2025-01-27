#pragma once

#include <cstdlib> // free, malloc, size_t
#include "util.h"

namespace glsl {

// Type-erasure
template <typename T>
static inline void astDestroy(void *self) {
    ((T*)self)->~T();
    free(self);
}

struct astMemory {
    astMemory() : data(nullptr), dtor(nullptr) { }
    template <typename T>
    explicit astMemory(T *data) : data((void*)data), dtor(&astDestroy<T>) { }
    void *data;
    void (*dtor)(void*);
    void destroy() const {
        dtor(data);
    }
};

struct astBase {
    enum astBaseType {
        kType,
        kVariable,
        kLayoutQualifier,
        kFunction,
        kDeclaration,
        kStatement,
        kExpression,
    } astType;
    explicit astBase(enum astBaseType type);
};

// Nodes are to inherit from astNode or astCollector
template <typename T>
struct astNode : astBase {
    explicit astNode(enum astBaseType type) : astBase(type) {}
    void *operator new(size_t size, vector<astMemory> *collector) noexcept {
        void *data = malloc(size);
        if (data)
            collector->push_back(astMemory((T*)data));
        return data;
    }
private:
    void *operator new(size_t) noexcept { return nullptr; }
    void operator delete(void *) {}
};


struct astFunction;
struct astType;
struct astGlobalVariable;
struct astExpression;
struct astLayoutQualifier;
struct astStatement;
struct astStruct;
struct astInterfaceBlock;
struct astVersionDirective;
struct astExtensionDirective;
struct astVariable;
struct astDefineStatement;
struct astElseDirectiveStatement;

struct astTU {
    explicit astTU(int type);

    enum {
        kCompute,
        kVertex,
        kTessControl,
        kTessEvaluation,
        kGeometry,
        kFragment
    };

    int type;

    astVersionDirective* versionDirective;
    vector<astExtensionDirective*> extensionDirectives;
    vector<astStatement*> statements;
    vector<astFunction*> functions;
    vector<astGlobalVariable*> globals;
    vector<astStruct*> structures;
    vector<astInterfaceBlock*> interfaceBlocks;

    // nodes as read off the file
    vector<astBase*> nodes;

private:
    astTU(const astTU&);
    astTU &operator=(const astTU&);
};

struct astType : astNode<astType> {
    enum astTypeType {
        kBuiltin,
        kStruct,
        kInterfaceBlock,
        kVersionDirective,
        kExtensionDirective
    } typeType;
    explicit astType(enum astTypeType type);
};

struct astBuiltin : astType {
    explicit astBuiltin(int type);
    int type; // kKeyword_*
};

struct astStruct : astType {
    astStruct();
    char *name;
    vector<astVariable*> fields;
};

struct astInterfaceBlock : astType {
    astInterfaceBlock();
    char *name;
    int storage; // one of the storage qualifiers: kIn, kOut, kUniform, kBuffer
    vector<astVariable*> fields;
};

struct astVersionDirective : astType {
    astVersionDirective();
    int version;
    int type;
};

struct astExtensionDirective : astType {
    astExtensionDirective();
    char *name;
    int behavior;
};

typedef astExpression astConstantExpression;

enum {
    kHighp,
    kMediump,
    kLowp
};

struct astVariable : astNode<astVariable> {
    enum astVariableType {
        kFunction,
        kParameter,
        kGlobal,
        kField
    } type;
    explicit astVariable(enum astVariableType type);
    char *name;
    struct astType *baseType;
    bool isArray;
    bool isPrecise;
    vector<astConstantExpression *> arraySizes;
};

struct astFunctionVariable : astVariable {
    astFunctionVariable();
    bool isConst;
    astExpression *initialValue;
};

// Storage qualifiers
enum storageType {
    kStorageUnknown,
    kConst,
    kIn,
    kOut,
    kInOut,
    kAttribute,
    kUniform,
    kVarying,
    kBuffer,
    kShared
};

// Auxiliary storage qualifiers
enum auxiliaryType {
    kAuxiliaryUnknown,
    kCentroid,
    kSample,
    kPatch,
};

// Memory qualifiers
enum {
    kCoherent = 1 << 0,
    kVolatile = 1 << 1,
    kRestrict = 1 << 2,
    kReadOnly = 1 << 3,
    kWriteOnly = 1 << 4
};

struct astFunctionParameter : astVariable {
    astFunctionParameter();
    int storage; // in or out only
    int auxiliary;
    int memory;
    int precision;
};

enum {
    kSmooth,
    kFlat,
    kNoPerspective
};

struct astGlobalVariable : astVariable {
    astGlobalVariable();
    int storage;
    int auxiliary;
    int memory;
    int precision;
    int interpolation;
    bool isInvariant;
    astConstantExpression *initialValue;
    vector<astLayoutQualifier*> layoutQualifiers;
};

struct astLayoutQualifier : astNode<astLayoutQualifier> {
    astLayoutQualifier();
    char *name;
    astConstantExpression *initialValue;
};

struct astFunction : astNode<astFunction> {
    astFunction();
    struct astType *returnType;
    char *name;
    vector<astFunctionParameter*> parameters;
    vector<astStatement*> statements;
    bool isPrototype;
};

struct astDeclaration : astNode<astDeclaration> {
    astDeclaration();
    astVariable *variable;
};

struct astStatement : astNode<astStatement> {
    enum astStatementType {
        kCompound,
        kEmpty,
        kDeclaration,
        kExpression,
        kIf,
        kSwitch,
        kCaseLabel,
        kWhile,
        kDo,
        kFor,
        kContinue,
        kBreak,
        kReturn,
        kDiscard,
        kDefine,
        kIfDirective,
        kElseDirective,
        kIfDefDirective,
        kIfNDefDirective,
        kEndIfDirective,
    } type;
    explicit astStatement(enum astStatementType type);
    [[nodiscard]] const char *name() const;
};

struct astSimpleStatement : astStatement {
    explicit astSimpleStatement(enum astStatementType type);
};

struct astCompoundStatement : astStatement {
    astCompoundStatement();
    vector<astStatement*> statements;
};

struct astDefineStatement : astSimpleStatement {
    astDefineStatement();
    char* name;
    vector<char*> parameters;
    astExpression* value;
};

struct astElseDirectiveStatement : astSimpleStatement {
    astElseDirectiveStatement();
    astExpression* value;
    vector<astBase*> elseNodes;
};

struct astIfDirectiveStatement : astStatement {
    explicit astIfDirectiveStatement(astStatementType type = astStatement::kIfDirective);
    astExpression* value;
    vector<astBase*> thenNodes;
    vector<astBase*> elseNodes;
};

struct astIfDefDirectiveStatement : astIfDirectiveStatement {
    astIfDefDirectiveStatement();
};

struct astIfNDefDirectiveStatement : astIfDirectiveStatement {
    astIfNDefDirectiveStatement();
};

struct astEndIfDirectiveStatement : astSimpleStatement {
    astEndIfDirectiveStatement();
};

struct astEmptyStatement : astSimpleStatement {
    astEmptyStatement();
};

struct astDeclarationStatement : astSimpleStatement {
    astDeclarationStatement();
    vector<astFunctionVariable*> variables;
};

struct astExpressionStatement : astSimpleStatement {
    explicit astExpressionStatement(astExpression *expression);
    astExpression *expression;
};

struct astIfStatement : astSimpleStatement {
    astIfStatement();
    astExpression *condition;
    astStatement *thenStatement;
    astStatement *elseStatement;
};

struct astSwitchStatement : astSimpleStatement {
    astSwitchStatement();
    astExpression *expression;
    vector<astStatement*> statements;
};

struct astCaseLabelStatement : astSimpleStatement {
    astCaseLabelStatement();
    astConstantExpression *condition;
    bool isDefault;
};

struct astIterationStatement : astSimpleStatement {
    explicit astIterationStatement(enum astStatementType type);
};

struct astWhileStatement : astIterationStatement {
    astWhileStatement();
    astSimpleStatement *condition; // astExpressionStatement or astDeclarationStatement only
    astStatement *body;
};

struct astDoStatement : astIterationStatement {
    astDoStatement();
    astStatement *body;
    astExpression *condition;
};

struct astForStatement : astIterationStatement {
    astForStatement();
    astSimpleStatement *init; // astExpressionStatement or astDeclarationStatement only
    astExpression *condition;
    astExpression *loop;
    astStatement *body;
};

struct astJumpStatement : astStatement {
    explicit astJumpStatement(enum astStatementType type);
};

struct astContinueStatement : astJumpStatement {
    astContinueStatement();
};

struct astBreakStatement : astJumpStatement {
    astBreakStatement();
};

struct astReturnStatement : astJumpStatement {
    astReturnStatement();
    astExpression *expression;
};

struct astDiscardStatement : astJumpStatement {
    astDiscardStatement();
};

struct astExpression : astNode<astExpression> {
    // Base class
    enum astExpressionType {
        kIntConstant,
        kUIntConstant,
        kFloatConstant,
        kDoubleConstant,
        kBoolConstant,
        kVariableIdentifier,
        kDefineIdentifier,
        kUnknownIdentifier,
        kFieldOrSwizzle,
        kArraySubscript,
        kFunctionCall,
        kConstructorCall,
        kPostIncrement,
        kPostDecrement,
        kUnaryMinus,
        kUnaryPlus,
        kBitNot,
        kLogicalNot,
        kPrefixIncrement,
        kPrefixDecrement,
        kSequence,
        kAssign,
        kOperation,
        kTernary
    } type;
    explicit astExpression(enum astExpressionType type);
};

struct astIntConstant : astExpression {
    explicit astIntConstant(int value);
    int value;
};

struct astUIntConstant : astExpression {
    explicit astUIntConstant(unsigned int value);
    unsigned int value;
};

struct astFloatConstant : astExpression {
    explicit astFloatConstant(float value);
    float value;
};

struct astDoubleConstant : astExpression {
    explicit astDoubleConstant(double value);
    double value;
};

struct astBoolConstant : astExpression {
    explicit astBoolConstant(bool value);
    bool value;
};

struct astVariableIdentifier : astExpression {
    explicit astVariableIdentifier(astVariable *variable);
    astVariable *variable;
};

struct astDefineIdentifier : astExpression {
    explicit astDefineIdentifier(astDefineStatement *define);
    astDefineStatement *define;
};

struct astUnknownIdentifier : astExpression {
    explicit astUnknownIdentifier(char *define);
    char *define;
};

struct astFieldOrSwizzle : astExpression {
    astFieldOrSwizzle();
    astExpression *operand;
    char *name;
};

struct astArraySubscript : astExpression {
    astArraySubscript();
    astExpression *operand;
    astExpression *index;
};

struct astFunctionCall : astExpression {
    astFunctionCall();
    char *name;
    vector<astExpression*> parameters;
};

struct astConstructorCall : astExpression {
    astConstructorCall();
    struct astType *type;
    vector<astExpression*> parameters;
};

struct astUnaryExpression : astExpression {
    // Base class
    astUnaryExpression(enum astExpressionType type, astExpression *operand);
    astExpression *operand;
};

struct astBinaryExpression : astExpression {
    // Base class
    explicit astBinaryExpression(enum astExpressionType type);
    astExpression *operand1;
    astExpression *operand2;
};

struct astPostIncrementExpression : astUnaryExpression {
    explicit astPostIncrementExpression(astExpression *operand);
};

struct astPostDecrementExpression : astUnaryExpression {
    explicit astPostDecrementExpression(astExpression *operand);
};

struct astUnaryPlusExpression : astUnaryExpression {
    explicit astUnaryPlusExpression(astExpression *operand);
};

struct astUnaryMinusExpression : astUnaryExpression {
    explicit astUnaryMinusExpression(astExpression *operand);
};

struct astUnaryBitNotExpression : astUnaryExpression {
    explicit astUnaryBitNotExpression(astExpression *operand);
};

struct astUnaryLogicalNotExpression : astUnaryExpression {
    explicit astUnaryLogicalNotExpression(astExpression *operand);
};

struct astPrefixIncrementExpression : astUnaryExpression {
    explicit astPrefixIncrementExpression(astExpression *operand);
};

struct astPrefixDecrementExpression : astUnaryExpression {
    explicit astPrefixDecrementExpression(astExpression *operand);
};

struct astSequenceExpression : astBinaryExpression {
    astSequenceExpression();
};

struct astAssignmentExpression : astBinaryExpression {
    explicit astAssignmentExpression(int assignment);
    int assignment;
};

struct astOperationExpression : astBinaryExpression {
    explicit astOperationExpression(int operation);
    int operation;
};

struct astTernaryExpression : astExpression {
    astTernaryExpression();
    astExpression *condition;
    astExpression *onTrue;
    astExpression *onFalse;
};

}
