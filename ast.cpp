#include "ast.h"

namespace glsl {

const char *astStatement::name() const {
    switch (type) {
    case kCompound:     return "compound";
    case kDeclaration:  return "declaration";
    case kExpression:   return "expression";
    case kIf:           return "if";
    case kSwitch:       return "switch";
    case kCaseLabel:    return "case label";
    case kWhile:        return "while";
    case kDo:           return "do";
    case kFor:          return "for";
    case kContinue:     return "continue";
    case kBreak:        return "break";
    case kReturn:       return "return";
    case kDiscard:      return "discard";
    case kInclude:      return "include";
    case kIfDefDirective:  return "ifdef directive";
    case kIfNDefDirective: return "ifndef directive";
    case kElseDirective:  return "else directive";
    case kDefine:       return "define";
    case kEndIfDirective: return "endif directive";
    case kEmpty:        return "empty";
    default:            return "(unknown)";
    }
}

astTU::astTU(int type)
    : type(type)
    , versionDirective(0)
{
}

astBase::astBase(enum astBaseType type)
    : astType(type)
{
}

astType::astType(enum astTypeType type)
    : astNode<astType>(kType)
    , typeType(type)
{
}

astStruct::astStruct()
    : astType(kStruct)
    , name(0)
{
}

astInterfaceBlock::astInterfaceBlock()
    : astType(kInterfaceBlock)
    , name(0)
    , storage(0)
{
}

astExtensionDirective::astExtensionDirective()
    : astType(kExtensionDirective)
    , name(0)
    , behavior(-1)
{
}

astVersionDirective::astVersionDirective()
    : astType(kVersionDirective)
    , version(-1)
    , type(-1)
{
}

astBuiltin::astBuiltin(int type)
    : astType(kBuiltin)
    , type(type)
{
}

astVariable::astVariable(enum astVariableType type)
    : astNode<astVariable>(kVariable)
    , type(type)
    , name(0)
    , baseType(0)
    , isArray(false)
    , isPrecise(false)
{
}

astFunctionVariable::astFunctionVariable()
    : astVariable(astVariable::kFunction)
    , isConst(false)
    , initialValue(0)
{
}

astFunctionParameter::astFunctionParameter()
    : astVariable(astVariable::kParameter)
    , storage(-1)
    , auxiliary(-1)
    , memory(0)
    , precision(-1)
{
}

astGlobalVariable::astGlobalVariable()
    : astVariable(astVariable::kGlobal)
    , storage(-1)
    , auxiliary(-1)
    , memory(0)
    , precision(-1)
    , interpolation(-1)
    , isInvariant(false)
    , initialValue(0)
{
}

astSimpleStatement::astSimpleStatement(enum astStatementType type)
    : astStatement(type)
{
}

astCompoundStatement::astCompoundStatement()
    : astStatement(astStatement::kCompound)
{
}

astDefineStatement::astDefineStatement()
    : astSimpleStatement (astStatement::kDefine)
    , name (0)
    , value (0)
{
}

astElseDirectiveStatement::astElseDirectiveStatement()
    : astSimpleStatement (astStatement::kElseDirective)
    , value (0)
{
}

astIncludeStatement::astIncludeStatement()
    : astSimpleStatement (astStatement::kInclude)
    , name (0)
{
}

astIfDefDirectiveStatement::astIfDefDirectiveStatement()
    : astIfDirectiveStatement (astStatement::kIfDefDirective)
{
}

astIfNDefDirectiveStatement::astIfNDefDirectiveStatement()
    : astIfDirectiveStatement (astStatement::kIfNDefDirective)
{
}

astIfDirectiveStatement::astIfDirectiveStatement(astStatementType type)
    : astStatement (type)
    , value (0)
{
}

    astEndIfDirectiveStatement::astEndIfDirectiveStatement()
    : astSimpleStatement(astStatement::kEndIfDirective)
{
}

astEmptyStatement::astEmptyStatement()
    : astSimpleStatement(astStatement::kEmpty)
{
}

astDeclarationStatement::astDeclarationStatement()
    : astSimpleStatement(astStatement::kDeclaration)
{
}

astLayoutQualifier::astLayoutQualifier()
    : astNode<astLayoutQualifier>(kLayoutQualifier)
    , name(0)
    , initialValue(0)
{
}

astFunction::astFunction()
    : astNode<astFunction>(kFunction)
    , returnType(0)
    , name(0)
    , isPrototype(false)
{
}

astDeclaration::astDeclaration()
    : astNode<astDeclaration>(kDeclaration)
    , variable(0)
{
}

astStatement::astStatement(enum astStatementType type)
    : astNode<astStatement>(kStatement)
    , type(type)
{
}

astExpressionStatement::astExpressionStatement(astExpression *expression)
    : astSimpleStatement(astStatement::kExpression)
    , expression(expression)
{
}

astIfStatement::astIfStatement()
    : astSimpleStatement(astStatement::kIf)
    , condition(0)
    , thenStatement(0)
    , elseStatement(0)
{
}

astSwitchStatement::astSwitchStatement()
    : astSimpleStatement(astStatement::kSwitch)
    , expression(0)
{
}

astCaseLabelStatement::astCaseLabelStatement()
    : astSimpleStatement(astStatement::kCaseLabel)
    , condition(0)
    , isDefault(false)
{
}

astIterationStatement::astIterationStatement(enum astStatementType type)
    : astSimpleStatement(type)
{
}

astWhileStatement::astWhileStatement()
    : astIterationStatement(astStatement::kWhile)
    , condition(0)
    , body(0)
{
}

astDoStatement::astDoStatement()
    : astIterationStatement(astStatement::kDo)
    , body(0)
    , condition(0)
{
}

astForStatement::astForStatement()
    : astIterationStatement(astStatement::kFor)
    , init(0)
    , condition(0)
    , loop(0)
    , body(0)
{
}

astJumpStatement::astJumpStatement(enum astStatementType type)
    : astStatement(type)
{
}

astContinueStatement::astContinueStatement()
    : astJumpStatement(astStatement::kContinue)
{
}

astBreakStatement::astBreakStatement()
    : astJumpStatement(astStatement::kBreak)
{
}

astReturnStatement::astReturnStatement()
    : astJumpStatement(astStatement::kReturn)
    , expression(0)
{
}

astDiscardStatement::astDiscardStatement()
    : astJumpStatement(astStatement::kDiscard)
{
}

astExpression::astExpression(enum astExpressionType type)
    : astNode<astExpression>(kExpression)
    , type(type)
{
}

astIntConstant::astIntConstant(int value)
    : astExpression(astExpression::kIntConstant)
    , value(value)
{
}

astUIntConstant::astUIntConstant(unsigned int value)
    : astExpression(astExpression::kUIntConstant)
    , value(value)
{
}

astFloatConstant::astFloatConstant(float value)
    : astExpression(astExpression::kFloatConstant)
    , value(value)
{
}

astDoubleConstant::astDoubleConstant(double value)
    : astExpression(astExpression::kDoubleConstant)
    , value(value)
{
}

astBoolConstant::astBoolConstant(bool value)
    : astExpression(astExpression::kBoolConstant)
    , value(value)
{
}

astVariableIdentifier::astVariableIdentifier(astVariable *variable)
    : astExpression(astExpression::kVariableIdentifier)
    , variable(variable)
{
}

astDefineIdentifier::astDefineIdentifier(astDefineStatement *define)
    : astExpression(astExpression::kDefineIdentifier)
    , define(define)
{
}

astUnknownIdentifier::astUnknownIdentifier(char* name)
    : astExpression(astExpression::kUnknownIdentifier)
    , define(name)
{
}

astFieldOrSwizzle::astFieldOrSwizzle()
    : astExpression(kFieldOrSwizzle)
    , operand(0)
    , name(0)
{
}

astArraySubscript::astArraySubscript()
    : astExpression(kArraySubscript)
    , operand(0)
    , index(0)
{
}

astFunctionCall::astFunctionCall()
    : astExpression(astExpression::kFunctionCall)
    , name(0)
{
}

astConstructorCall::astConstructorCall()
    : astExpression(astExpression::kConstructorCall)
    , type(0)
{
}

astUnaryExpression::astUnaryExpression(enum astExpressionType type, astExpression *operand)
    : astExpression(type)
    , operand(operand)
{
}

astBinaryExpression::astBinaryExpression(enum astExpressionType type)
    : astExpression(type)
    , operand1(0)
    , operand2(0)
{
}

astPostIncrementExpression::astPostIncrementExpression(astExpression *operand)
    : astUnaryExpression(astExpression::kPostIncrement, operand)
{
}

astPostDecrementExpression::astPostDecrementExpression(astExpression *operand)
    : astUnaryExpression(astExpression::kPostDecrement, operand)
{
}

astUnaryPlusExpression::astUnaryPlusExpression(astExpression *operand)
    : astUnaryExpression(astExpression::kUnaryPlus, operand)
{
}

astUnaryMinusExpression::astUnaryMinusExpression(astExpression *operand)
    : astUnaryExpression(astExpression::kUnaryMinus, operand)
{
}

astUnaryBitNotExpression::astUnaryBitNotExpression(astExpression *operand)
    : astUnaryExpression(astExpression::kBitNot, operand)
{
}

astUnaryLogicalNotExpression::astUnaryLogicalNotExpression(astExpression *operand)
    : astUnaryExpression(astExpression::kLogicalNot, operand)
{
}

astPrefixIncrementExpression::astPrefixIncrementExpression(astExpression *operand)
    : astUnaryExpression(astExpression::kPrefixIncrement, operand)
{
}

astPrefixDecrementExpression::astPrefixDecrementExpression(astExpression *operand)
    : astUnaryExpression(astExpression::kPrefixDecrement, operand)
{
}

astSequenceExpression::astSequenceExpression()
    : astBinaryExpression(astExpression::kSequence)
{
}

astAssignmentExpression::astAssignmentExpression(int assignment)
    : astBinaryExpression(astExpression::kAssign)
    , assignment(assignment)
{
}

astOperationExpression::astOperationExpression(int operation)
    : astBinaryExpression(astExpression::kOperation)
    , operation(operation)
{
}

astTernaryExpression::astTernaryExpression()
    : astExpression(astExpression::kTernary)
    , condition(0)
    , onTrue(0)
    , onFalse(0)
{
}

}
