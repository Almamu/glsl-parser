#ifndef AST_HDR
#define AST_HDR
#include <stdlib.h> // free, malloc, size_t
#include "util.h"

namespace glsl {

// Type-erasure
template <typename T>
static inline void astDestroy(void *self) {
    ((T*)self)->~T();
    free(self);
}

struct astMemory {
    astMemory() : data(0), dtor(0) { }
    template <typename T>
    astMemory(T *data) : data((void*)data), dtor(&astDestroy<T>) { }
    void *data;
    void (*dtor)(void*);
    void destroy() {
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
    astBase(enum astBaseType type);
};

// Nodes are to inherit from astNode or astCollector
template <typename T>
struct astNode : astBase {
    astNode(enum astBaseType type) : astBase(type) {}
    void *operator new(size_t size, vector<astMemory> *collector) throw() {
        void *data = malloc(size);
        if (data)
            collector->push_back(astMemory((T*)data));
        return data;
    }
private:
    void *operator new(size_t);
    void operator delete(void *);
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
    astTU(int type);

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
    astType(enum astTypeType type);
};

struct astBuiltin : astType {
    astBuiltin(int type);
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
    astVariable(enum astVariableType type);
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
        kInclude,
        kIfDirective,
        kElseDirective,
        kIfDefDirective,
        kIfNDefDirective,
        kEndIfDirective,
    } type;
    astStatement(enum astStatementType type);
    const char *name() const;
};

struct astSimpleStatement : astStatement {
    astSimpleStatement(enum astStatementType type);
};

struct astCompoundStatement : astStatement {
    astCompoundStatement();
    vector<astStatement*> statements;
};

struct astIncludeStatement : astSimpleStatement {
    astIncludeStatement();
    char* name;
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
    astIfDirectiveStatement(astStatementType type = astStatement::kIfDirective);
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
    astExpressionStatement(astExpression *expression);
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
    astIterationStatement(enum astStatementType type);
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
    astJumpStatement(enum astStatementType type);
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
    astExpression(enum astExpressionType type);
};

struct astIntConstant : astExpression {
    astIntConstant(int value);
    int value;
};

struct astUIntConstant : astExpression {
    astUIntConstant(unsigned int value);
    unsigned int value;
};

struct astFloatConstant : astExpression {
    astFloatConstant(float value);
    float value;
};

struct astDoubleConstant : astExpression {
    astDoubleConstant(double value);
    double value;
};

struct astBoolConstant : astExpression {
    astBoolConstant(bool value);
    bool value;
};

struct astVariableIdentifier : astExpression {
    astVariableIdentifier(astVariable *variable);
    astVariable *variable;
};

struct astDefineIdentifier : astExpression {
    astDefineIdentifier(astDefineStatement *define);
    astDefineStatement *define;
};

struct astUnknownIdentifier : astExpression {
    astUnknownIdentifier(char *define);
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
    astBinaryExpression(enum astExpressionType type);
    astExpression *operand1;
    astExpression *operand2;
};

struct astPostIncrementExpression : astUnaryExpression {
    astPostIncrementExpression(astExpression *operand);
};

struct astPostDecrementExpression : astUnaryExpression {
    astPostDecrementExpression(astExpression *operand);
};

struct astUnaryPlusExpression : astUnaryExpression {
    astUnaryPlusExpression(astExpression *operand);
};

struct astUnaryMinusExpression : astUnaryExpression {
    astUnaryMinusExpression(astExpression *operand);
};

struct astUnaryBitNotExpression : astUnaryExpression {
    astUnaryBitNotExpression(astExpression *operand);
};

struct astUnaryLogicalNotExpression : astUnaryExpression {
    astUnaryLogicalNotExpression(astExpression *operand);
};

struct astPrefixIncrementExpression : astUnaryExpression {
    astPrefixIncrementExpression(astExpression *operand);
};

struct astPrefixDecrementExpression : astUnaryExpression {
    astPrefixDecrementExpression(astExpression *operand);
};

struct astSequenceExpression : astBinaryExpression {
    astSequenceExpression();
};

struct astAssignmentExpression : astBinaryExpression {
    astAssignmentExpression(int assignment);
    int assignment;
};

struct astOperationExpression : astBinaryExpression {
    astOperationExpression(int operation);
    int operation;
};

struct astTernaryExpression : astExpression {
    astTernaryExpression();
    astExpression *condition;
    astExpression *onTrue;
    astExpression *onFalse;
};

}

#endif
